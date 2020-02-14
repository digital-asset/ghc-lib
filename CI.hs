-- Copyright (c) 2019-2020, Digital Asset (Switzerland) GmbH and/or
-- its affiliates. All rights reserved.  SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

-- CI script, compatible with all of Travis, Appveyor and Azure.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}
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
    Options { upload, ghcFlavor, stackOptions } <- Opts.execParser opts
    version <- buildDists ghcFlavor stackOptions
    when upload $ bintrayUpload version

data Options = Options
    { upload :: Bool
    , ghcFlavor :: GhcFlavor
    , stackOptions :: StackOptions
    } deriving (Show)

data StackOptions = StackOptions
    { resolver :: Maybe String  -- If 'Just _', override stack.yaml.
    , verbosity :: Maybe String -- If provided, pass '--verbosity=xxx' to 'stack build'. Valid values are "silent",  "error", "warn", "info" or "debug".
    , cabalVerbose :: Bool -- If enabled, pass '--cabal-verbose' to 'stack build'.
    , ghcOptions :: Maybe String -- If 'Just _', pass '--ghc-options="xxx"' to 'stack build' (for ghc verbose, try 'v3').
    } deriving (Show)

data GhcFlavor = Ghc8101 | Ghc881 | Ghc882 | DaGhc881 | GhcMaster String
  deriving (Eq, Show)

-- Last tested gitlab.haskell.org/ghc/ghc.git at
current :: String
current = "1bfd825943f6077836361ddc788fb40f2421e283" -- 2020-02-13

-- Command line argument generators.

stackResolverOpt :: Maybe String -> String
stackResolverOpt = \case
  Just resolver -> "--resolver " ++ resolver
  Nothing -> ""

ghcFlavorOpt :: GhcFlavor -> String
ghcFlavorOpt = \case
    Ghc8101 -> "--ghc-flavor ghc-8.10.1"
    Ghc881 -> "--ghc-flavor ghc-8.8.1"
    Ghc882 -> "--ghc-flavor ghc-8.8.2"
    DaGhc881 -> "--ghc-flavor da-ghc-8.8.1"
    GhcMaster _hash -> "--ghc-flavor ghc-master"
      -- The git SHA1 hash is not passed to ghc-lib-gen at this time.

stackVerbosityOpt :: Maybe String -> String
stackVerbosityOpt = \case
  Just verbosity -> "--verbosity=" ++ verbosity
  Nothing -> ""

cabalVerboseOpt :: Bool -> String
cabalVerboseOpt True = "--cabal-verbose"; cabalVerboseOpt False = ""

ghcOptionsOpt :: Maybe String -> String
ghcOptionsOpt = \case
  Just options -> "--ghc-options=\"" ++ options ++ "\""
  Nothing -> ""

-- Calculate a version string based on a date and a ghc flavor.
genVersionStr :: GhcFlavor -> (Day -> String)
genVersionStr = \case
   GhcMaster _ -> \day -> "0." ++ replace "-" "" (showGregorian day)
   Ghc8101     -> \day -> "8.10.1." ++ replace "-" "" (showGregorian day)
   Ghc882      -> \day -> "8.8.2." ++ replace "-" "" (showGregorian day)
   _           -> \day -> "8.8.1." ++ replace "-" "" (showGregorian day)

parseOptions :: Opts.Parser Options
parseOptions = Options
    <$> Opts.switch
        ( Opts.long "upload-to-bintray"
       <> Opts.help "If specified, will try uploading the sdists to Bintray, using credentials in BINTRAY_BASIC_AUTH env var.")
    <*> Opts.option readFlavor
        ( Opts.long "ghc-flavor"
       <> Opts.help "The ghc-flavor to test against"
        )
    <*> parseStackOptions
 where
   readFlavor :: Opts.ReadM GhcFlavor
   readFlavor = Opts.eitherReader $ \case
       "ghc-8.10.1" -> Right Ghc8101
       "ghc-8.8.1" -> Right Ghc881
       "ghc-8.8.2" -> Right Ghc882
       "da-ghc-8.8.1" -> Right DaGhc881
       "ghc-master" -> Right (GhcMaster current)
       hash -> Right (GhcMaster hash)

parseStackOptions :: Opts.Parser StackOptions
parseStackOptions = StackOptions
    <$> Opts.optional ( Opts.strOption
        ( Opts.long "resolver"
       <> Opts.help "If specified, pass '--resolver=xxx' to stack"
        ))
    <*> Opts.optional ( Opts.strOption
        ( Opts.long "verbosity"
       <> Opts.help "If specified, pass '--verbosity=xxx' to stack"
        ))
    <*> Opts.switch
        ( Opts.long "cabal-verbose"
       <> Opts.help "If specified, pass '--cabal-verbose' to stack"
        )
    <*> Opts.optional ( Opts.strOption
        ( Opts.long "ghc-options"
       <> Opts.help "If specified, pass '--ghc-options=\"xxx\"' to stack"
        ))

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

buildDists :: GhcFlavor -> StackOptions -> IO String
buildDists ghcFlavor
  StackOptions {resolver, verbosity, cabalVerbose, ghcOptions} =
  do
    -- Clear up any detritus left over from previous runs.
    toDelete <- (["ghc", "ghc-lib", "ghc-lib-parser"] ++) .
      filter (isExtensionOf ".tar.gz") <$> getDirectoryContents "."
    forM_ toDelete removePath
    cmd "git checkout stack.yaml"

    -- Get packages missing on Windows needed by hadrian.
    when isWindows $
        stack "exec -- pacman -S autoconf automake-wrapper make patch python tar --noconfirm"
    -- Building of hadrian dependencies that result from the
    -- invocations of ghc-lib-gen can require some versions of these
    -- have been installed.
    stack "build alex happy"

    -- Get a clone of ghc.
    cmd "git clone https://gitlab.haskell.org/ghc/ghc.git"
    case ghcFlavor of
        Ghc8101 -> cmd "cd ghc && git fetch --tags && git checkout ghc-8.10.1-rc1"
        Ghc881 -> cmd "cd ghc && git fetch --tags && git checkout ghc-8.8.1-release"
        Ghc882 -> cmd "cd ghc && git fetch --tags && git checkout ghc-8.8.2-release"
        DaGhc881 -> do
            cmd "cd ghc && git fetch --tags && git checkout ghc-8.8.1-release"
            -- Apply Digital Asset extensions.
            cmd "cd ghc && git remote add upstream https://github.com/digital-asset/ghc.git"
            cmd "cd ghc && git fetch upstream"
            cmd "cd ghc && git -c \"user.name=Cookie Monster\" -c \"user.email=cookie.monster@seasame-street.com\" merge --no-edit upstream/da-master-8.8.1 upstream/da-unit-ids-8.8.1"
        GhcMaster hash -> cmd $ "cd ghc && git checkout " ++ hash
    cmd "cd ghc && git submodule update --init --recursive"

    -- Feedback on the compiler used for ghc-lib-gen.
    stack "exec -- ghc --version"
    -- Build ghc-lib-gen. Do this here rather than in the Azure script
    -- so that it's not forgotten when testing this program locally.
    stack "--no-terminal build"

    -- Any invocations of GHC in the sdist steps that follow use the
    -- hadrian/stack.yaml resolver (which can and we should expect
    -- to be, different to our resolver).

    -- Calculate verison and package names.
    version <- tag
    let pkg_ghclib = "ghc-lib-" ++ version
        pkg_ghclib_parser = "ghc-lib-parser-" ++ version

    -- Make and extract an sdist of ghc-lib-parser.
    appendFile "ghc/hadrian/stack.yaml" $ unlines ["ghc-options:","  \"$everything\": -O0 -j"]
    -- Feedback on the compiler used for ghc-lib-gen.
    stack $ "exec -- ghc-lib-gen ghc --ghc-lib-parser " ++ ghcFlavorOpt ghcFlavor
    patchVersion version "ghc/ghc-lib-parser.cabal"
    mkTarball pkg_ghclib_parser
    renameDirectory pkg_ghclib_parser "ghc-lib-parser"
    removeFile "ghc/ghc-lib-parser.cabal"
    cmd "git checkout stack.yaml"

    -- Make and extract an sdist of ghc-lib.
    cmd "cd ghc && git checkout ."
    appendFile "ghc/hadrian/stack.yaml" $ unlines ["ghc-options:","  \"$everything\": -O0 -j"]
    -- Feedback on the compiler used for ghc-lib-gen.
    stack $ "exec -- ghc-lib-gen ghc --ghc-lib " ++ ghcFlavorOpt ghcFlavor
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
              , "- examples/strip-locs"
              ] ++
      case ghcFlavor of
        GhcMaster _ ->
          -- Resolver 'lts-12.26' serves 'transformers-0.5.5.0' which
          -- lacks 'Control.Monad.Trans.RWS.CPS'. The need for that
          -- module came in around around 09/20/2019. Putting this here
          -- keeps the CI ghc-8.4.4 builds going.
          unlines ["extra-deps: [transformers-0.5.6.2]"]
        DaGhc881 ->
          unlines ["flags: {mini-compile: {daml-unit-ids: true}}"]
        _ -> ""

    -- All invocations of GHC from here on are using our resolver.

    -- Feedback on what compiler has been selected for building
    -- ghc-lib packages and tests.
    stack "exec -- ghc --version"

    -- Separate the two library build commands so they are
    -- independently timed. Note that optimizations in these builds
    -- are disabled in stack.yaml via `ghc-options: -O0`.
    stack $ "--no-terminal --interleaved-output " ++ "build " ++ ghcOptionsOpt ghcOptions  ++ " ghc-lib-parser"
    stack $ "--no-terminal --interleaved-output " ++ "build " ++ ghcOptionsOpt ghcOptions  ++ " ghc-lib"
    stack $ "--no-terminal --interleaved-output build " ++ ghcOptionsOpt ghcOptions  ++ " mini-hlint mini-compile strip-locs"

    -- Run tests.
    stack "--no-terminal exec -- mini-hlint examples/mini-hlint/test/MiniHlintTest.hs"
    stack "--no-terminal exec -- mini-hlint examples/mini-hlint/test/MiniHlintTest_fatal_error.hs"
    stack "--no-terminal exec -- mini-hlint examples/mini-hlint/test/MiniHlintTest_non_fatal_error.hs"
    stack "--no-terminal exec -- mini-hlint examples/mini-hlint/test/MiniHlintTest_respect_dynamic_pragma.hs"
    stack "--no-terminal exec -- mini-hlint examples/mini-hlint/test/MiniHlintTest_fail_unknown_pragma.hs"
    stack "--no-terminal exec -- strip-locs examples/mini-compile/test/MiniCompileTest.hs"
    stack "--no-terminal exec -- mini-compile examples/mini-compile/test/MiniCompileTest.hs"

#if __GLASGOW_HASKELL__ == 808 && \
    (__GLASGOW_HASKELL_PATCHLEVEL1__ == 1 || __GLASGOW_HASKELL_PATCHLEVEL1__ == 2) && \
    defined (mingw32_HOST_OS)
    -- Skip these tests on ghc-8.8.1 (exclusively). See
    -- https://gitlab.haskell.org/ghc/ghc/issues/17599.
#else
    -- Test everything loads in GHCi, see
    -- https://github.com/digital-asset/ghc-lib/issues/27
    stack "--no-terminal exec -- ghc -ignore-dot-ghci -package=ghc-lib-parser -e \"print 1\""
    stack "--no-terminal exec -- ghc -ignore-dot-ghci -package=ghc-lib -e \"print 1\""
#endif

    -- Something like, "8.8.1.20190828".
    tag  -- The return value of type 'IO string'.

    where
      stack :: String -> IO ()
      stack action = cmd $ "stack " ++
        concatMap (<> " ")
                 [ stackResolverOpt resolver
                 , stackVerbosityOpt verbosity
                 , cabalVerboseOpt cabalVerbose
                 ] ++
        action

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
        stack "sdist ghc --tar-dir=."
        cmd $ "tar -xvf " ++ target ++ ".tar.gz"

      tag :: IO String
      tag = do
        UTCTime day _ <- getCurrentTime
        return $ genVersionStr ghcFlavor day

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
          putStrLn $ "# Removing " ++ p
          removePathForcibly p
