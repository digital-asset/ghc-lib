#!/usr/bin/env stack --resolver lts-12.10 runhaskell --package extra --package optparse-applicative --
-- Copyright (c) 2019, Digital Asset (Switzerland) GmbH and/or its
-- affiliates. All rights reserved.  SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

-- CI script, compatible with all of Travis, Appveyor and Azure.

import Control.Monad.Extra
import System.Directory
import System.FilePath
import System.IO.Extra
import System.Info.Extra
import System.Process.Extra
import System.Time.Extra
import Data.List.Extra
import Data.Time.Clock
import Data.Time.Calendar
import Data.Semigroup ((<>))
import qualified Options.Applicative as Opts
import qualified System.Environment as Env
import qualified System.Exit as Exit

main :: IO ()
main = do
    let opts = Opts.info (parseOptions Opts.<**> Opts.helper)
                 ( Opts.fullDesc
                <> Opts.progDesc "Build (and possibly upload) ghc-lib and ghc-lib-parser tarballs."
                <> Opts.header "CI - CI script for ghc-lib" )
    Options { upload = upload }  <- Opts.execParser opts
    version <- buildDists
    when upload $ bintrayUpload version

newtype Options = Options { upload :: Bool }

parseOptions :: Opts.Parser Options
parseOptions = Options
    <$> Opts.switch
        ( Opts.long "upload-to-bintray"
       <> Opts.help "If specified, will try uploading the sdists to Bintray, using credentials in BINTRAY_BASIC_AUTH env var.")

bintrayUpload :: String -> IO ()
bintrayUpload version = do
    credentials <- Env.lookupEnv "BINTRAY_BASIC_AUTH"
    case credentials of
      Nothing ->
        Exit.die $ unlines ["Error: Cannot upload without BINTRAY_BASIC_AUTH.",
                            "To set the environment variable, run:",
                            "    BINTRAY_BASIC_AUTH=<creds> ./CI.hs --upload-to-bintray",
                            "where <creds> should be of the form:",
                            "  fname.lname@digitalassetsdk:0123456789abcdef0123456789abcdef01234567",
                            "You can find your API key (the part after the colon) at:",
                            "  https://bintray.com/profile/edit",
                            "after logging in. (The username is also displayed on that page.)"]
      Just creds -> do
        cmd $ concat ["curl -T ./ghc-lib-parser-", version, ".tar.gz",
                          " -u", creds,
                          " https://api.bintray.com/content/digitalassetsdk/ghc-lib/da-ghc-lib/", version, "/ghc-lib-parser-", version, ".tar.gz"]
        cmd $ concat ["curl -T ./ghc-lib-", version, ".tar.gz",
                          " -u", creds,
                          " https://api.bintray.com/content/digitalassetsdk/ghc-lib/da-ghc-lib/", version, "/ghc-lib-", version, ".tar.gz"]
        cmd $ concat ["curl -X POST",
                          " -u", creds,
                          " https://api.bintray.com/content/digitalassetsdk/ghc-lib/da-ghc-lib/", version, "/publish"]
        where
          cmd :: String -> IO ()
          cmd x = do
            let c = replace creds "***:***" x
            putStrLn $ "\n\n# Running: " ++ c
            hFlush stdout
            (t, _) <- duration $ callCommand x
            putStrLn $ "# Completed in " ++ showDuration t ++ ": " ++ c ++ "\n"
            hFlush stdout

buildDists :: IO String
buildDists = do
    -- Get packages missing on Windows needed by hadrian.
    when isWindows $
        cmd "stack exec -- pacman -S autoconf automake-wrapper make patch python tar --noconfirm"

    -- Clear up any detritus left over from previous runs.
    toDelete <- (["ghc", "ghc-lib", "ghc-lib-parser"] ++) .
      filter (isExtensionOf ".tar.gz") <$> getDirectoryContents "."
    forM_ toDelete removePath
    cmd "git checkout stack.yaml"

    -- Get a clone of ghc.
    cmd "git clone https://gitlab.haskell.org/ghc/ghc.git"
    cmd "cd ghc && git fetch --tags && git checkout ghc-8.8.1-release"
    -- Apply Digital Asset extensions.
    cmd "cd ghc && git remote add upstream git@github.com:digital-asset/ghc.git"
    cmd "cd ghc && git fetch upstream"
    cmd "cd ghc && git merge --no-edit upstream/da-master-8.8.1 upstream/da-unit-ids-8.8.1"
    cmd "cd ghc && git submodule update --init --recursive"
    appendFile "ghc/hadrian/stack.yaml" $ unlines ["ghc-options:","  \"$everything\": -O0 -j"]

    -- Build ghc-lib-gen. Do this here rather than in the Azure script
    -- so that it's not forgotten when testing this program locally.
    cmd "stack --no-terminal build"

    -- Calculate verison and package names.
    version <- tag
    let pkg_ghclib = "ghc-lib-" ++ version
        pkg_ghclib_parser = "ghc-lib-parser-" ++ version

    -- Make and extract an sdist of ghc-lib-parser.
    cmd "stack exec -- ghc-lib-gen ghc --ghc-lib-parser"
    patchVersion version "ghc/ghc-lib-parser.cabal"
    mkTarball pkg_ghclib_parser
    renameDirectory pkg_ghclib_parser "ghc-lib-parser"
    removeFile "ghc/ghc-lib-parser.cabal"
    cmd "git checkout stack.yaml"

    -- Make and extract an sdist of ghc-lib.
    cmd "cd ghc && git checkout ."
    appendFile "ghc/hadrian/stack.yaml" $ unlines ["ghc-options:","  \"$everything\": -O0 -j"]
    cmd "stack exec -- ghc-lib-gen ghc --ghc-lib"
    patchVersion version "ghc/ghc-lib.cabal"
    patchConstraint version "ghc/ghc-lib.cabal"
    mkTarball pkg_ghclib
    renameDirectory pkg_ghclib "ghc-lib"
    removeFile "ghc/ghc-lib.cabal"
    cmd "git checkout stack.yaml"

    -- Append the libraries and examples to stack.yaml.
    stackYaml <- readFile' "stack.yaml"
    writeFile "stack.yaml" $
      stackYaml ++
      unlines [ "- ghc-lib-parser"
              , "- ghc-lib"
              , "- examples/mini-hlint"
              , "- examples/mini-compile"
              ]

    -- Separate the two library build commands so they are
    -- independently timed. Note that optimizations in these builds
    -- are disabled in stack.yaml via `ghc-options: -O0`.
    cmd "stack build ghc-lib-parser --no-terminal --interleaved-output"
    cmd "stack build ghc-lib --no-terminal --interleaved-output"
    cmd "stack build mini-hlint mini-compile --no-terminal --interleaved-output"

    -- Run tests.
    cmd "stack exec --no-terminal -- mini-hlint examples/mini-hlint/test/MiniHlintTest.hs"
    cmd "stack exec --no-terminal -- mini-hlint examples/mini-hlint/test/MiniHlintTest_fatal_error.hs"
    cmd "stack exec --no-terminal -- mini-hlint examples/mini-hlint/test/MiniHlintTest_non_fatal_error.hs"
    cmd "stack exec --no-terminal -- mini-compile examples/mini-compile/test/MiniCompileTest.hs"
    -- Test everything loads in GHCi, see
    -- https://github.com/digital-asset/ghc-lib/issues/27
    cmd "stack exec --no-terminal -- ghc -package=ghc-lib-parser -e \"print 1\""
    cmd "stack exec --no-terminal -- ghc -package=ghc-lib -e \"print 1\""
    tag
    where
      cmd :: String -> IO ()
      cmd x = do
        putStrLn $ "\n\n# Running: " ++ x
        hFlush stdout
        (t, _) <- duration $ system_ x
        putStrLn $ "# Completed in " ++ showDuration t ++ ": " ++ x ++ "\n"
        hFlush stdout

      mkTarball :: String -> IO ()
      mkTarball target = do
        writeFile "stack.yaml" . (++ "- ghc\n") =<< readFile' "stack.yaml"
        cmd "stack sdist ghc --tar-dir=."
        cmd $ "tar -xvf " ++ target ++ ".tar.gz"

      tag :: IO String
      tag = do
        UTCTime day _ <- getCurrentTime
        return $ "8.8.1." ++ replace "-" "" (showGregorian day)

      patchVersion :: String -> FilePath -> IO ()
      patchVersion version file =
        writeFile file .
          replace "version: 0.1.0" ("version: " ++ version)
          =<< readFile' file

      patchConstraint :: String -> FilePath -> IO ()
      patchConstraint version file =
        writeFile file .
          replace "ghc-lib-parser" ("ghc-lib-parser == " ++ version)
          =<< readFile' file

      removePath :: FilePath -> IO ()
      removePath p =
        whenM (doesPathExist p) $ do
          putStrLn ("# Removing " ++ p)
          removePathForcibly p
