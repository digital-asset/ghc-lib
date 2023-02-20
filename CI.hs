-- Copyright (c) 2019-2022, Digital Asset (Switzerland) GmbH and/or
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
import System.Info (os, arch)
import System.Exit
import System.Environment
import Data.Maybe
import Data.List.Extra
import Data.Time.Clock
import Data.Time.Calendar
import qualified Options.Applicative as Opts

main :: IO ()
main = do
    let opts =
          Opts.info (parseOptions Opts.<**> Opts.helper)
          ( Opts.fullDesc
            <> Opts.progDesc "Build ghc-lib and ghc-lib-parser tarballs."
            <> Opts.header "CI - CI script for ghc-lib"
          )
    Options { ghcFlavor, noGhcCheckout, noBuilds, stackOptions, versionSuffix } <- Opts.execParser opts
    version <- buildDists ghcFlavor noGhcCheckout noBuilds stackOptions versionSuffix
    putStrLn version

data Options = Options
    { ghcFlavor :: GhcFlavor
    , noGhcCheckout :: Bool
    , noBuilds :: Bool
    , stackOptions :: StackOptions
    , versionSuffix :: Maybe String
    } deriving (Show)

data StackOptions = StackOptions
    { stackYaml :: Maybe String -- Optional config file
    , resolver :: Maybe String  -- If 'Just _', override stack.yaml.
    , verbosity :: Maybe String -- If provided, pass '--verbosity=xxx' to 'stack build'. Valid values are "silent",  "error", "warn", "info" or "debug".
    , cabalVerbose :: Bool -- If enabled, pass '--cabal-verbose' to 'stack build'.
    , ghcOptions :: Maybe String -- If 'Just _', pass '--ghc-options="xxx"' to 'stack build' (for ghc verbose, try 'v3').
    } deriving (Show)

data GhcFlavor = Da DaFlavor
               | GhcMaster String
               | Ghc961
               | Ghc944 | Ghc943 | Ghc942 | Ghc941
               | Ghc926 | Ghc925 | Ghc924 | Ghc923 | Ghc922 | Ghc921
               | Ghc902 | Ghc901
               | Ghc8107 | Ghc8106 | Ghc8105 | Ghc8104 | Ghc8103 | Ghc8102 | Ghc8101 | Ghc881
               | Ghc884 | Ghc883  | Ghc882
  deriving (Eq, Show)

data DaFlavor = DaFlavor
  { mergeBaseSha :: String
  , patches :: [String]
  , cpp :: [String]
  , flavor :: String
  , upstream :: String
  }
  deriving (Eq, Show)

-- Last tested gitlab.haskell.org/ghc/ghc.git at
current :: String
current = "7612dc713d5a1f108cfd6eb731435b090fbb8809" -- 2023-02-04

-- Command line argument generators.

stackYamlOpt :: Maybe FilePath -> String
stackYamlOpt = \case
  Just stackYaml -> "--stack-yaml " ++ stackYaml
  Nothing -> ""

stackResolverOpt :: Maybe String -> String
stackResolverOpt = \case
  Just resolver -> "--resolver " ++ resolver
  Nothing -> ""

ghcFlavorOpt :: GhcFlavor -> String
ghcFlavorOpt = \case
    Ghc961 -> "--ghc-flavor ghc-9.6.1"
    Ghc944 -> "--ghc-flavor ghc-9.4.4"
    Ghc943 -> "--ghc-flavor ghc-9.4.3"
    Ghc942 -> "--ghc-flavor ghc-9.4.2"
    Ghc941 -> "--ghc-flavor ghc-9.4.1"
    Ghc926 -> "--ghc-flavor ghc-9.2.6"
    Ghc925 -> "--ghc-flavor ghc-9.2.5"
    Ghc924 -> "--ghc-flavor ghc-9.2.4"
    Ghc923 -> "--ghc-flavor ghc-9.2.3"
    Ghc922 -> "--ghc-flavor ghc-9.2.2"
    Ghc921 -> "--ghc-flavor ghc-9.2.1"
    Ghc901 -> "--ghc-flavor ghc-9.0.1"
    Ghc902 -> "--ghc-flavor ghc-9.0.2"
    Ghc8101 -> "--ghc-flavor ghc-8.10.1"
    Ghc8102 -> "--ghc-flavor ghc-8.10.2"
    Ghc8103 -> "--ghc-flavor ghc-8.10.3"
    Ghc8104 -> "--ghc-flavor ghc-8.10.4"
    Ghc8105 -> "--ghc-flavor ghc-8.10.5"
    Ghc8106 -> "--ghc-flavor ghc-8.10.6"
    Ghc8107 -> "--ghc-flavor ghc-8.10.7"
    Ghc881 -> "--ghc-flavor ghc-8.8.1"
    Ghc882 -> "--ghc-flavor ghc-8.8.2"
    Ghc883 -> "--ghc-flavor ghc-8.8.3"
    Ghc884 -> "--ghc-flavor ghc-8.8.4"
    Da DaFlavor {flavor} -> "--ghc-flavor " <> flavor
    GhcMaster _hash -> "--ghc-flavor ghc-master"
      -- The git SHA1 hash is not passed to ghc-lib-gen at this time.

cppOpts :: GhcFlavor -> String
cppOpts (Da DaFlavor {cpp}) = unwords $ concat [["--cpp", v] | v <- cpp]
cppOpts _ = ""

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

genDateSuffix :: IO String
genDateSuffix = do
  UTCTime day _ <- getCurrentTime
  pure $ replace "-" "" (showGregorian day)

-- Calculate a version string based on a ghc flavor and a suffix.
genVersionStr :: GhcFlavor -> String -> String
genVersionStr flavor suffix =
  base <> case suffix of
    "" -> ""
    _ -> "." <> suffix
  where
    base = case flavor of
      Da {}       -> "8.8.1"
      GhcMaster _ -> "0"
      Ghc961      -> "9.6.1"
      Ghc944      -> "9.4.4"
      Ghc943      -> "9.4.3"
      Ghc942      -> "9.4.2"
      Ghc941      -> "9.4.1"
      Ghc926      -> "9.2.6"
      Ghc925      -> "9.2.5"
      Ghc924      -> "9.2.4"
      Ghc923      -> "9.2.3"
      Ghc922      -> "9.2.2"
      Ghc921      -> "9.2.1"
      Ghc901      -> "9.0.1"
      Ghc902      -> "9.0.2"
      Ghc8101     -> "8.10.1"
      Ghc8102     -> "8.10.2"
      Ghc8103     -> "8.10.3"
      Ghc8104     -> "8.10.4"
      Ghc8105     -> "8.10.5"
      Ghc8106     -> "8.10.6"
      Ghc8107     -> "8.10.7"
      Ghc881      -> "8.8.1"
      Ghc882      -> "8.8.2"
      Ghc883      -> "8.8.3"
      Ghc884      -> "8.8.4"

parseOptions :: Opts.Parser Options
parseOptions = Options
    <$> ((Da <$> parseDaOptions)
         Opts.<|>
         Opts.option readFlavor
          ( Opts.long "ghc-flavor"
         <> Opts.help "The ghc-flavor to test against"
          )
        )
    <*> Opts.switch
          ( Opts.long "no-checkout"
          <> Opts.help "If enabled, don't perform a GHC checkout"
          )
    <*> Opts.switch
          ( Opts.long "no-builds"
          <> Opts.help "If enabled, don't build & test packages & examples"
          )
    <*> parseStackOptions
    <*> Opts.optional
          ( Opts.strOption
            ( Opts.long "version-suffix"
            <> Opts.help "If specified, append to version string for generated ghc-lib. Otherwise use current date."
            )
          )
 where
   readFlavor :: Opts.ReadM GhcFlavor
   readFlavor = Opts.eitherReader $ \case
       "ghc-9.6.1" -> Right Ghc961
       "ghc-9.4.4" -> Right Ghc944
       "ghc-9.4.3" -> Right Ghc943
       "ghc-9.4.2" -> Right Ghc942
       "ghc-9.4.1" -> Right Ghc941
       "ghc-9.2.6" -> Right Ghc926
       "ghc-9.2.5" -> Right Ghc925
       "ghc-9.2.4" -> Right Ghc924
       "ghc-9.2.3" -> Right Ghc923
       "ghc-9.2.2" -> Right Ghc922
       "ghc-9.2.1" -> Right Ghc921
       "ghc-9.0.1" -> Right Ghc901
       "ghc-9.0.2" -> Right Ghc902
       "ghc-8.10.1" -> Right Ghc8101
       "ghc-8.10.2" -> Right Ghc8102
       "ghc-8.10.3" -> Right Ghc8103
       "ghc-8.10.4" -> Right Ghc8104
       "ghc-8.10.5" -> Right Ghc8105
       "ghc-8.10.6" -> Right Ghc8106
       "ghc-8.10.7" -> Right Ghc8107
       "ghc-8.8.1" -> Right Ghc881
       "ghc-8.8.2" -> Right Ghc882
       "ghc-8.8.3" -> Right Ghc883
       "ghc-8.8.4" -> Right Ghc884
       "ghc-master" -> Right (GhcMaster current)
       hash -> Right (GhcMaster hash)
   parseDaOptions :: Opts.Parser DaFlavor
   parseDaOptions =
       Opts.flag' DaFlavor ( Opts.long "da" <> Opts.help "Enables DA custom build." )
       <*> Opts.strOption
           ( Opts.long "merge-base-sha"
          <> Opts.help "DA flavor only. Base commit to use from the GHC repo."
          <> Opts.showDefault
          <> Opts.value "ghc-8.8.1-release"
           )
       <*> (Opts.some
             (Opts.strOption
              ( Opts.long "patch"
             <> Opts.help "DA flavor only. Commits to merge in from the DA GHC fork, referenced as 'upstream'. Can be specified multiple times. If no patch is specified, default will be equivalent to `--patch upstream/da-master-8.8.1`. Specifying any patch will overwrite the default (i.e. replace, not add)."
              ))
            Opts.<|>
            pure ["upstream/da-master-8.8.1"])
       <*> (Opts.some
             (Opts.strOption
              ( Opts.long "cpp"
             <> Opts.help "DA flavor only. CPP flags to pass on to ghc-lib-gen. Can be specified multiple times. If no flags are specified, default will be equivalent to `--cpp -DDAML_PRIM`. Specifying any flag will overwrite the default (i.e. replace, not add)."
              ))
            Opts.<|>
            pure ["-DDAML_PRIM"])
       <*> Opts.strOption
           ( Opts.long "gen-flavor"
          <> Opts.help "DA flavor only. Flavor to pass on to ghc-lib-gen."
          <> Opts.showDefault
          <> Opts.value "da-ghc-8.8.1")
       <*> Opts.strOption
           ( Opts.long "upstream"
          <> Opts.help "DA flavor only. URL for the git remote add command."
          <> Opts.showDefault
          <> Opts.value "https://github.com/digital-asset/ghc.git")

parseStackOptions :: Opts.Parser StackOptions
parseStackOptions = StackOptions
    <$> Opts.optional ( Opts.strOption
        ( Opts.long "stack-yaml"
       <> Opts.help "If specified, pass '--stack-yaml=xxx' to stack"
        ))
    <*> Opts.optional ( Opts.strOption
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

buildDists :: GhcFlavor -> Bool -> Bool -> StackOptions -> Maybe String -> IO String
buildDists
  ghcFlavor
  noGhcCheckout
  noBuilds
  StackOptions {stackYaml, resolver, verbosity, cabalVerbose, ghcOptions}
  versionSuffix
  =
  do
    let stackConfig = fromMaybe "stack.yaml" stackYaml

    -- Clean up old state.
    isCi <- isJust <$> lookupEnv "GHCLIB_AZURE"
    -- Avoid https://github.com/commercialhaskell/stack/issues/5866.
    unless isCi $ stack "clean --full" -- Recursively delete '.stack-work'
    filesInDot <- getDirectoryContents "."
    let lockFiles = filter (isExtensionOf ".lock") filesInDot
        tarBalls  = filter (isExtensionOf ".tar.gz") filesInDot
        ghcDirs   = ["ghc" | not noGhcCheckout] ++ [ "ghc-lib", "ghc-lib-parser" ]
        toDelete  = ghcDirs ++ tarBalls ++ lockFiles
    forM_ toDelete removePath
    cmd $ "git checkout " ++ stackConfig
    cmd "git checkout ghc-lib-gen.cabal examples"

    -- Get packages missing on Windows needed by hadrian.
    when isWindows $
        stack "exec -- pacman -S autoconf automake-wrapper make patch python tar mintty --noconfirm"
    -- Building of hadrian dependencies that result from the
    -- invocations of ghc-lib-gen can require some versions of these
    -- have been installed.
    stack "build alex happy"

    -- If '--no-checkout' is given, it's on the caller to get the GHC
    -- clone with e.g.
    --  git clone https://gitlab.haskell.org/ghc/ghc.git && \
    --      git fetch --tags && git submodule update --init --recursive
    -- and it won't be deleted between runs.
    if noGhcCheckout then
      cmd "cd ghc && git clean -xdf && git submodule foreach git clean -xdf && git submodule foreach git checkout . && git checkout ."
    else do
      cmd "git clone https://gitlab.haskell.org/ghc/ghc.git"
      cmd "cd ghc && git fetch --tags"
    gitCheckout ghcFlavor

    -- Feedback on the compiler used for ghc-lib-gen.
    stack "exec -- ghc --version"
    -- Build ghc-lib-gen. Do this here rather than in the Azure script
    -- so that it's not forgotten when testing this program locally.
    stack "build --no-terminal --ghc-options \"-Wall -Wno-name-shadowing -Werror\""

    -- Any invocations of GHC in the sdist steps that follow use the
    -- hadrian/stack.yaml resolver (which can and we should expect
    -- to be, different to our resolver).

    -- Calculate version and package names.
    version <- tag
    let pkg_ghclib = "ghc-lib-" ++ version
        pkg_ghclib_parser = "ghc-lib-parser-" ++ version

    -- Make and extract an sdist of ghc-lib-parser.
    cmd "cd ghc && git checkout ."
    stack $ "exec -- ghc-lib-gen ghc --ghc-lib-parser " ++ ghcFlavorOpt ghcFlavor ++ " " ++ cppOpts ghcFlavor ++ " " ++ stackResolverOpt resolver
    patchVersion version "ghc/ghc-lib-parser.cabal"
    mkTarball pkg_ghclib_parser
    renameDirectory pkg_ghclib_parser "ghc-lib-parser"
    removeFile "ghc/ghc-lib-parser.cabal"
    cmd "git checkout stack.yaml"

    -- Make and extract an sdist of ghc-lib.
    stack $ "exec -- ghc-lib-gen ghc --ghc-lib " ++ ghcFlavorOpt ghcFlavor ++ " " ++ cppOpts ghcFlavor ++ " " ++ stackResolverOpt resolver ++ " " ++ "--skip-init"
    patchVersion version "ghc/ghc-lib.cabal"
    patchConstraints version "ghc/ghc-lib.cabal"
    mkTarball pkg_ghclib
    renameDirectory pkg_ghclib "ghc-lib"
    removeFile "ghc/ghc-lib.cabal"
    cmd "git checkout stack.yaml"

    patchVersion version "ghc-lib-gen.cabal"
    patchVersion version "examples/ghc-lib-test-utils/ghc-lib-test-utils.cabal"
    patchConstraints version "examples/ghc-lib-test-utils/ghc-lib-test-utils.cabal"
    patchVersion version "examples/ghc-lib-test-mini-hlint/ghc-lib-test-mini-hlint.cabal"
    patchConstraints version "examples/ghc-lib-test-mini-hlint/ghc-lib-test-mini-hlint.cabal"
    patchVersion version "examples/ghc-lib-test-mini-compile/ghc-lib-test-mini-compile.cabal"
    patchConstraints version "examples/ghc-lib-test-mini-compile/ghc-lib-test-mini-compile.cabal"

    verifyConstraint "ghc-lib-parser == " version "ghc-lib/ghc-lib.cabal"
    verifyConstraint "ghc-lib-parser == " version "examples/ghc-lib-test-mini-hlint/ghc-lib-test-mini-hlint.cabal"
    verifyConstraint "ghc-lib-test-utils == " version "examples/ghc-lib-test-mini-hlint/ghc-lib-test-mini-hlint.cabal"
    verifyConstraint "ghc-lib-parser == " version "examples/ghc-lib-test-mini-compile/ghc-lib-test-mini-compile.cabal"
    verifyConstraint "ghc-lib == " version "examples/ghc-lib-test-mini-compile/ghc-lib-test-mini-compile.cabal"

    stack "sdist . --ignore-check --tar-dir=."
    stack "sdist examples/ghc-lib-test-utils --tar-dir=."
    stack "sdist examples/ghc-lib-test-mini-hlint --tar-dir=."
    stack "sdist examples/ghc-lib-test-mini-compile --tar-dir=."

    when noBuilds exitSuccess

    -- Append the libraries and examples to the prevailing stack
    -- configuration file.
    stackYamlFileContents <- readFile' stackConfig
    writeFile stackConfig $
      stackYamlFileContents ++
      unlines [ "- ghc-lib-parser"
              , "- ghc-lib"
              , "- examples/ghc-lib-test-utils"
              , "- examples/ghc-lib-test-mini-hlint"
              , "- examples/ghc-lib-test-mini-compile"
              ] ++
      case ghcFlavor of
#if __GLASGOW_HASKELL__ == 804 && __GLASGOW_HASKELL_PATCHLEVEL1__ == 4
        GhcMaster _ ->
          -- Resolver 'lts-12.26' serves 'transformers-0.5.5.0' which
          -- lacks 'Control.Monad.Trans.RWS.CPS'. The need for that
          -- module came in around around 09/20/2019. Putting this
          -- here keeps the CI ghc-8.4.4 builds going (for 8.8.*
          -- ghc-libs, there is no support for bootstrapping ghc-8.10
          -- builds with ghc-8.4.4; see azure-pipelines.yml for an
          -- explanation)
          unlines ["extra-deps: [transformers-0.5.6.2]"]
#endif
        Da {} ->
          unlines ["flags: {ghc-lib-test-mini-compile: {daml-unit-ids: true}}"]
        _ -> ""

    -- All invocations of GHC from here on are using our resolver.

    -- Feedback on what compiler has been selected for building
    -- ghc-lib packages and tests.
    stack "ghc -- --version"

    -- Separate the two library build commands so they are
    -- independently timed. Note that optimizations in these builds
    -- are disabled in stack.yaml via `ghc-options: -O0`.
    -- `-haddock` makes the parser stricter about Haddock comments (see
    -- https://gitlab.haskell.org/ghc/ghc/-/commit/c35c545d3f32f092c52052349f741739a844ec0f).
    -- TODO: https://github.com/digital-asset/ghc/issues/97
    let ghcOpts = case ghcFlavor of Da {} -> ghcOptionsOpt ghcOptions;  _ -> ghcOptionsWithHaddock ghcOptions
    stack $ "--no-terminal --interleaved-output build " ++ ghcOpts ++ " ghc-lib-parser"
    stack $ "--no-terminal --interleaved-output build " ++ ghcOptionsOpt ghcOptions ++ " ghc-lib"
    stack $ "--no-terminal --interleaved-output build " ++ ghcOptionsOpt ghcOptions ++ " ghc-lib-test-mini-hlint ghc-lib-test-mini-compile"

    miniHlintCmdFile <- writeCmdFile "ghc-lib-test-mini-hlint" stackConfig resolver
    stack $ "test ghc-lib-test-mini-hlint --no-terminal " ++ testArguments miniHlintCmdFile stackConfig resolver ghcFlavor

    miniCompileCmdFile <- writeCmdFile "ghc-lib-test-mini-compile" stackConfig resolver
    stack $ "test ghc-lib-test-mini-compile --no-terminal " ++ testArguments miniCompileCmdFile stackConfig resolver ghcFlavor

#if __GLASGOW_HASKELL__ == 808 && \
    (__GLASGOW_HASKELL_PATCHLEVEL1__ == 1 || __GLASGOW_HASKELL_PATCHLEVEL1__ == 2) && \
    defined (mingw32_HOST_OS)
    -- Skip these tests on ghc-8.8.1 and ghc-8.8.2. See
    -- https://gitlab.haskell.org/ghc/ghc/issues/17599.
#else
     -- Missing `SymI_HasProto(setKeepCAFs)` in 'rts/RtsSymbols.c'
     -- prevents loading in GHCi on Windows (see
     -- https://gitlab.haskell.org/ghc/ghc/-/issues/22961). I don't
     -- know why it so far only exhibits with 9.2.6. Seems to me it
     -- should be a problem with >= ghc-9.6.1 too (but, "if it ain't
     -- broke don't fix it").
    unless (ghcFlavor == Ghc926 && System.Info.Extra.isWindows) $ do
      -- Test everything loads in GHCi, see
      -- https://github.com/digital-asset/ghc-lib/issues/27
      stack "--no-terminal ghc -- -ignore-dot-ghci -package=ghc-lib-parser -e \"print 1\""
      stack "--no-terminal ghc -- -ignore-dot-ghci -package=ghc-lib -e \"print 1\""
#endif

    -- Something like, "8.8.1.20190828".
    tag -- The return value of type 'IO string'.

    where

      writeCmdFile :: String -> FilePath -> Maybe String -> IO FilePath
      writeCmdFile exe stackConfig resolver = do
        let filename = exe
        let cmd = "stack " ++ stackYamlOpt (Just $ "../.." </> stackConfig) ++ " " ++ stackResolverOpt resolver ++ " " ++ "exec -- " ++ exe ++ " "
        writeFile filename cmd
        pure filename

      testArguments :: FilePath -> FilePath -> Maybe String -> GhcFlavor -> String
      testArguments cmdFile stackConfig resolver ghcFlavor =
        "--test-arguments " ++
        "\"" ++
        "--test-command " ++ "../../" </> cmdFile ++ " " ++
        stackYamlOpt (Just $ "../.." </> stackConfig) ++ " " ++
        stackResolverOpt resolver ++ " " ++ ghcFlavorOpt ghcFlavor ++ " " ++
        "\""

      ghcOptionsWithHaddock :: Maybe String -> String
      -- Enabling strict haddock mode with -haddock (and for some
      -- build compilers -Winvalid-haddock) has become too tedious.
      -- See 20935, 20924 and now 21269. The good news is that MR#
      -- 7762 means this shouldn't be an issue anymore going forward.
      ghcOptionsWithHaddock = ghcOptionsOpt

      -- Mitigate against macOS/ghc-9.2.2 failures for lack of this
      -- c-include path. See
      -- https://gitlab.haskell.org/ghc/ghc/-/issues/20592#note_391266.
      -- There are reports that this exhibits with 9.0.2 and 9.2.1 as
      -- well but I haven't observed that.
      prelude :: (String, String) -> String
#if __GLASGOW_HASKELL__ == 902 && __GLASGOW_HASKELL_PATCHLEVEL1__ == 2
      prelude ("darwin", _) = "C_INCLUDE_PATH=`xcrun --show-sdk-path`/usr/include/ffi"
#endif
      prelude _ = ""

      stack :: String -> IO ()
      stack action = cmd $ prelude (os, arch) ++ " stack " ++
        concatMap (<> " ")
                 [ stackYamlOpt stackYaml
                 , stackResolverOpt resolver
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
        suffix <- maybe genDateSuffix pure versionSuffix
        return $ genVersionStr ghcFlavor suffix

      patchVersion :: String -> FilePath -> IO ()
      patchVersion version file =
        writeFile file .
          -- ghc-lib, ghc-lib-parser
          replace "version: 0.1.0" ("version: " ++ version) .
          -- ghc-lib-test-mini-hlint, ghc-lib-test-mini-compile
          replace "version:             0.1.0.0" ("version: " ++ version)
          =<< readFile' file

      patchConstraints :: String -> FilePath -> IO ()
      patchConstraints version file =
        writeFile file .
          -- affects ghc-lib.cabal
          replace "ghc-lib-parser\n" ("ghc-lib-parser == " ++ version ++ "\n") .
          -- affects ghc-lib-test-utils, ghc-lib-test-mini-hlint, ghc-lib-test-mini-compile
          replace ", ghc-lib-test-utils" (", ghc-lib-test-utils == " ++ version ++ "\n") .
          replace ", ghc-lib\n" (", ghc-lib == " ++ version ++ "\n") .
          replace ", ghc-lib-parser\n" (", ghc-lib-parser == " ++ version ++ "\n")
          =<< readFile' file

      verifyConstraint :: String -> String -> String -> IO ()
      verifyConstraint constraint version file = do
        res <- stripInfix constraint <$> readFile' file
        case res of
          Just (_, r) -> do
            case words r of
              v : _ -> unless (v == version) exitError
              _ -> exitError
          Nothing -> exitError
       where
         exitError = die $ file ++ ": " ++ constraint ++ version ++ " not satisfied"

      removePath :: FilePath -> IO ()
      removePath p =
        whenM (doesPathExist p) $ do
          putStrLn $ "# Removing " ++ p
          removePathForcibly p

      gitCheckout :: GhcFlavor -> IO ()
      gitCheckout ghcFlavor = do
        case ghcFlavor of
          Ghc961 -> cmd "cd ghc && git checkout ghc-9.6"
          Ghc944 -> cmd "cd ghc && git checkout ghc-9.4.4-release"
          Ghc943 -> cmd "cd ghc && git checkout ghc-9.4.3-release"
          Ghc942 -> cmd "cd ghc && git checkout ghc-9.4.2-release"
          Ghc941 -> cmd "cd ghc && git checkout ghc-9.4.1-release"
          Ghc926 -> cmd "cd ghc && git checkout ghc-9.2.6-release"
          Ghc925 -> cmd "cd ghc && git checkout ghc-9.2.5-release"
          Ghc924 -> cmd "cd ghc && git checkout ghc-9.2.4-release"
          Ghc923 -> cmd "cd ghc && git checkout ghc-9.2.3-release"
          Ghc922 -> cmd "cd ghc && git checkout ghc-9.2.2-release"
          Ghc921 -> cmd "cd ghc && git checkout ghc-9.2.1-release"
          Ghc901 -> cmd "cd ghc && git checkout ghc-9.0.1-release"
          Ghc902 -> cmd "cd ghc && git checkout ghc-9.0.2-release"
          Ghc8101 -> cmd "cd ghc && git checkout ghc-8.10.1-release"
          Ghc8102 -> cmd "cd ghc && git checkout ghc-8.10.2-release"
          Ghc8103 -> cmd "cd ghc && git checkout ghc-8.10.3-release"
          Ghc8104 -> cmd "cd ghc && git checkout ghc-8.10.4-release"
          Ghc8105 -> cmd "cd ghc && git checkout ghc-8.10.5-release"
          Ghc8106 -> cmd "cd ghc && git checkout ghc-8.10.6-release"
          Ghc8107 -> cmd "cd ghc && git checkout ghc-8.10.7-release"
          Ghc881 -> cmd "cd ghc && git checkout ghc-8.8.1-release"
          Ghc882 -> cmd "cd ghc && git checkout ghc-8.8.2-release"
          Ghc883 -> cmd "cd ghc && git checkout ghc-8.8.3-release"
          Ghc884 -> cmd "cd ghc && git checkout ghc-8.8.4-release"
          Da DaFlavor { mergeBaseSha, patches, upstream } -> do
              cmd $ "cd ghc && git checkout " <> mergeBaseSha
              -- Apply Digital Asset extensions.
              cmd $ "cd ghc && git remote add upstream " <> upstream
              cmd "cd ghc && git fetch upstream"
              cmd $ "cd ghc && git -c user.name=\"Cookie Monster\" -c user.email=cookie.monster@seasame-street.com merge --no-edit " <> unwords patches
          GhcMaster hash -> cmd $ "cd ghc && git checkout " ++ hash
        cmd "cd ghc && git submodule update --init --recursive"
