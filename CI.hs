#!/usr/bin/env stack --resolver lts-14.3 runhaskell --package extra --package optparse-applicative --
-- Copyright (c) 2019, Digital Asset (Switzerland) GmbH and/or its
-- affiliates. All rights reserved.  SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

-- CI script, compatible with all of Travis, Appveyor and Azure.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    let opts =
          Opts.info (parseOptions Opts.<**> Opts.helper)
          ( Opts.fullDesc
            <> Opts.progDesc "Build (and possibly upload) ghc-lib and ghc-lib-parser tarballs."
            <> Opts.header "CI - CI script for ghc-lib"
          )
    options@Options { upload, ghcFlavor, resolver }  <- Opts.execParser opts
    version <- buildDists ghcFlavor resolver
    when upload $ bintrayUpload version

data Options = Options
    { upload :: Bool
    , ghcFlavor :: GhcFlavor
    , resolver :: Maybe String -- If 'Just _', override stack.yaml.
    } deriving (Show)

stackResolverOpt :: Maybe String -> String
stackResolverOpt = \case
  Just resolver -> "--resolver " ++ resolver
  Nothing -> ""

data GhcFlavor = Ghc881 | DaGhc881 | GhcMaster
  deriving (Eq, Show)

ghcFlavorOpt :: GhcFlavor -> String
ghcFlavorOpt = \case
    Ghc881 -> "--ghc-flavor ghc-8.8.1"
    DaGhc881 -> "--ghc-flavor da-ghc-8.8.1"
    GhcMaster -> "--ghc-flavor ghc-master"

parseOptions :: Opts.Parser Options
parseOptions = Options
    <$> Opts.switch
        ( Opts.long "upload-to-bintray"
       <> Opts.help "If specified, will try uploading the sdists to Bintray, using credentials in BINTRAY_BASIC_AUTH env var.")
    <*> Opts.option readFlavor
        ( Opts.long "ghc-flavor"
       <> Opts.help "The ghc-flavor to test against"
        )
    <*> Opts.optional ( Opts.strOption
        ( Opts.long "resolver"
       <> Opts.help "If specified, the stack resolver to use"
        ))
 where
   readFlavor :: Opts.ReadM GhcFlavor
   readFlavor = Opts.eitherReader $ \case
       "ghc-8.8.1" -> Right Ghc881
       "da-ghc-8.8.1" -> Right DaGhc881
       "ghc-master" -> Right GhcMaster
       flavor -> Left $ "Failed to parse ghc flavor " <> show flavor <> " expected ghc-8.8.1 or da-ghc-8.8.1"

bintrayUpload :: String -> IO ()
bintrayUpload version = do
    credentials <- Env.lookupEnv "BINTRAY_BASIC_AUTH"
    case credentials of
      Nothing ->
        Exit.die $ unlines [
          "Error: Cannot upload without BINTRAY_BASIC_AUTH.",
          "To set the environment variable, run:",
          "    BINTRAY_BASIC_AUTH=<creds> ./CI.hs --upload-to-bintray",
          "where <creds> should be of the form:",
          "  fname.lname@digitalassetsdk:0123456789abcdef0123456789abcdef01234567",
          "You can find your API key (the part after the colon) at:",
          "  https://bintray.com/profile/edit",
          "after logging in. (The username is also displayed on that page.)"]
      Just creds -> do
        cmd $ concat [
            "curl -T ./ghc-lib-parser-", version, ".tar.gz",
            " -u", creds,
            " https://api.bintray.com/content/digitalassetsdk/ghc-lib/da-ghc-lib/", version, "/ghc-lib-parser-", version, ".tar.gz"]
        cmd $ concat [
            "curl -T ./ghc-lib-", version, ".tar.gz",
            " -u", creds,
            " https://api.bintray.com/content/digitalassetsdk/ghc-lib/da-ghc-lib/", version, "/ghc-lib-", version, ".tar.gz"]
        cmd $ concat [
            "curl -X POST",
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

buildDists :: GhcFlavor -> Maybe String -> IO String
buildDists ghcFlavor resolver = do
    -- One of "" (use 'stack.yaml') or, "--resolver=xxx".
    let resolverFlag=stackResolverOpt resolver

    -- Get packages missing on Windows needed by hadrian.
    when isWindows $
        cmd $ "stack exec "  ++ resolverFlag ++ " -- pacman -S autoconf automake-wrapper make patch python tar --noconfirm"

    -- Clear up any detritus left over from previous runs.
    toDelete <- (["ghc", "ghc-lib", "ghc-lib-parser"] ++) .
      filter (isExtensionOf ".tar.gz") <$> getDirectoryContents "."
    forM_ toDelete removePath
    cmd "git checkout stack.yaml"

    -- Get a clone of ghc.
    cmd "git clone https://gitlab.haskell.org/ghc/ghc.git"
    case ghcFlavor of
        Ghc881 ->
            cmd "cd ghc && git fetch --tags && git checkout ghc-8.8.1-release"
        DaGhc881 -> do
            cmd "cd ghc && git fetch --tags && git checkout ghc-8.8.1-release"
            -- Apply Digital Asset extensions.
            cmd "cd ghc && git remote add upstream https://github.com/digital-asset/ghc.git"
            cmd "cd ghc && git fetch upstream"
            cmd "cd ghc && git -c \"user.name=Doesn't Matter\" -c \"user.email=doesnt.matter@example.com\" merge --no-edit upstream/da-master-8.8.1 upstream/da-unit-ids-8.8.1"
        GhcMaster ->
            cmd "cd ghc && git checkout 11679e5bec1994775072e8e60f24b4ce104af0a7" -- 09/03/2019
    cmd "cd ghc && git submodule update --init --recursive"
    appendFile "ghc/hadrian/stack.yaml" $ unlines ["ghc-options:","  \"$everything\": -O0 -j"]


    -- Feedback on the compiler used for ghc-lib-gen.
    cmd $ "stack " ++ resolverFlag ++ " exec -- ghc --version"

    -- Build ghc-lib-gen. Do this here rather than in the Azure script
    -- so that it's not forgotten when testing this program locally.
    cmd $ "stack " ++ resolverFlag ++ " --no-terminal build"

    -- Calculate verison and package names.
    version <- tag
    let pkg_ghclib = "ghc-lib-" ++ version
        pkg_ghclib_parser = "ghc-lib-parser-" ++ version

    cmd $ "stack " ++ resolverFlag ++ " build alex happy"
    -- Building of hadrian dependencies that result from the
    -- invocations of ghc-lib-gen can require some versions of these
    -- have been installed.

    -- Any invocations of GHC in the sdist steps that follow use the
    -- hadrian/stack.yaml resolver (which can and we should expect
    -- to be, different to our resolver).

    -- Make and extract an sdist of ghc-lib-parser.
    cmd $ "stack " ++ resolverFlag ++ " exec -- ghc-lib-gen ghc --ghc-lib-parser " ++ ghcFlavorOpt ghcFlavor
    patchVersion version "ghc/ghc-lib-parser.cabal"
    mkTarball pkg_ghclib_parser resolverFlag
    renameDirectory pkg_ghclib_parser "ghc-lib-parser"
    removeFile "ghc/ghc-lib-parser.cabal"
    cmd "git checkout stack.yaml"

    -- Make and extract an sdist of ghc-lib.
    cmd "cd ghc && git checkout ."
    appendFile "ghc/hadrian/stack.yaml" $ unlines ["ghc-options:","  \"$everything\": -O0 -j"]
    cmd $ "stack " ++ resolverFlag ++ " exec -- ghc-lib-gen ghc --ghc-lib " ++ ghcFlavorOpt ghcFlavor
    patchVersion version "ghc/ghc-lib.cabal"
    patchConstraint version "ghc/ghc-lib.cabal"
    mkTarball pkg_ghclib resolverFlag
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
              , "- examples/strip-locs"
              ] ++
      if ghcFlavor == DaGhc881
        then unlines ["flags: {mini-compile: {daml-unit-ids: true}}"]
        else ""

    -- All GHC steps from here are using our resolver.

    -- Feedback on what compiler has been selected for building
    -- ghc-lib packages and tests.
    cmd $ "stack " ++ resolverFlag ++ " exec -- ghc --version"

    -- Separate the two library build commands so they are
    -- independently timed. Note that optimizations in these builds
    -- are disabled in stack.yaml via `ghc-options: -O0`.
    cmd $ "stack " ++ resolverFlag ++ " --no-terminal --interleaved-output " ++ "build ghc-lib-parser"
    cmd $ "stack " ++ resolverFlag ++ " --no-terminal --interleaved-output " ++ "build ghc-lib"
    cmd $ "stack " ++ resolverFlag ++ " build mini-hlint mini-compile strip-locs --no-terminal --interleaved-output"

    -- Run tests.
    cmd $ "stack " ++ resolverFlag ++ " --no-terminal exec -- mini-hlint examples/mini-hlint/test/MiniHlintTest.hs"
    cmd $ "stack " ++ resolverFlag ++ " --no-terminal exec -- mini-hlint examples/mini-hlint/test/MiniHlintTest_fatal_error.hs"
    cmd $ "stack " ++ resolverFlag ++ " --no-terminal exec -- mini-hlint examples/mini-hlint/test/MiniHlintTest_non_fatal_error.hs"
    cmd $ "stack " ++ resolverFlag ++ " --no-terminal exec -- mini-hlint examples/mini-hlint/test/MiniHlintTest_respect_dynamic_pragma.hs"
    cmd $ "stack " ++ resolverFlag ++ " --no-terminal exec -- mini-hlint examples/mini-hlint/test/MiniHlintTest_fail_unknown_pragma.hs"
    cmd $ "stack " ++ resolverFlag ++ " --no-terminal exec -- strip-locs examples/mini-compile/test/MiniCompileTest.hs"
    cmd $ "stack " ++ resolverFlag ++ " --no-terminal exec -- mini-compile examples/mini-compile/test/MiniCompileTest.hs"
    -- Test everything loads in GHCi, see
    -- https://github.com/digital-asset/ghc-lib/issues/27
    cmd $ "stack " ++ resolverFlag ++ " --no-terminal exec -- ghc -ignore-dot-ghci -package=ghc-lib-parser -e \"print 1\""
    cmd $ "stack " ++ resolverFlag ++ " --no-terminal exec -- ghc -ignore-dot-ghci -package=ghc-lib -e \"print 1\""

    -- Something like, "8.8.1.20190828".
    tag  -- The return value of type 'IO string'.

    where
      cmd :: String -> IO ()
      cmd x = do
        putStrLn $ "\n\n# Running: " ++ x
        hFlush stdout
        (t, _) <- duration $ system_ x
        putStrLn $ "# Completed in " ++ showDuration t ++ ": " ++ x ++ "\n"
        hFlush stdout

      mkTarball :: String -> String -> IO ()
      mkTarball target resolverFlag = do
        writeFile "stack.yaml" . (++ "- ghc\n") =<< readFile' "stack.yaml"
        cmd $ "stack " ++ resolverFlag ++ " sdist ghc --tar-dir=."
        cmd $ "tar -xvf " ++ target ++ ".tar.gz"

      tag :: IO String
      tag = do
        UTCTime day _ <- getCurrentTime
        return $ if ghcFlavor == GhcMaster
          then "0." ++ replace "-" "" (showGregorian day)
          else "8.8.1." ++ replace "-" "" (showGregorian day)

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
