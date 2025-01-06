-- Copyright (c) 2019-2024, Digital Asset (Switzerland) GmbH and/or
-- its affiliates. All rights reserved.  SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
#if __GLASGOW_HASKELL__ >= 902
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
#endif
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wx-partial #-}
#endif

module Ghclibgen
  ( applyPatchHeapClosures,
    applyPatchAclocal,
    applyPatchHsVersions,
    applyPatchGhcPrim,
    applyPatchHaddockHs,
    applyPatchRtsBytecodes,
    applyPatchGHCiInfoTable,
    applyPatchGHCiMessage,
    applyPatchDerivedConstants,
    applyPatchDisableCompileTimeOptimizations,
    applyPatchRtsIncludePaths,
    applyPatchStage,
    applyPatchNoMonoLocalBinds,
    applyPatchCmmParseNoImplicitPrelude,
    applyPatchCompilerGHCParserLexer,
    applyPatchCompilerGHCUnitTypes,
    applyPatchHadrianCabalProject,
    applyPatchGhcInternalEventWindowsHsc,
    applyPatchTemplateHaskellLanguageHaskellTHSyntax,
    applyPatchTemplateHaskellCabal,
    applyPatchFptoolsAlex,
    applyPatchFpFindCxxStdLib,
    generatePrerequisites,
    mangleCSymbols,
    generateGhcLibCabal,
    generateGhcLibParserCabal,
    setupModuleDepsPlaceholders,
  )
where

import Control.Exception (handle)
import Control.Monad.Extra
-- Make sure we generate / on all platforms.

import Data.Char
import Data.List.Extra hiding (find)
import qualified Data.List.NonEmpty
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GhclibgenFlavor
import System.Directory
import System.Directory.Extra
import System.FilePath hiding (dropTrailingPathSeparator, normalise, (</>))
import System.FilePath.Posix (dropTrailingPathSeparator, normalise, (</>))
import System.IO.Error (isEOFError)
import System.IO.Extra
import System.Process.Extra

-- Cabal files from libraries inside GHC that are merged together.
cabalFileLibraries :: GhcFlavor -> [FilePath]
cabalFileLibraries ghcFlavor =
  join
    [ [ "libraries/template-haskell/template-haskell.cabal",
        "libraries/ghc-heap/ghc-heap.cabal",
        "libraries/ghc-boot-th/ghc-boot-th.cabal",
        "libraries/ghc-boot/ghc-boot.cabal",
        "libraries/ghci/ghci.cabal",
        "compiler/ghc.cabal"
      ],
      ["libraries/ghc-platform/ghc-platform.cabal" | ghcSeries ghcFlavor > GHC_9_8]
    ]

-- C-preprocessor "include dirs" for 'ghc-lib-parser'.
ghcLibParserIncludeDirs :: GhcFlavor -> [FilePath]
ghcLibParserIncludeDirs ghcFlavor =
  join
    [ case ghcSeries ghcFlavor of
        series | series > GHC_9_10 -> ["libraries/ghc-internal/include", "rts/include", "rts/include/stg"]
        series | series > GHC_9_8 -> ["rts/include", "rts/include/stg"]
        series | series >= GHC_9_4 -> ["rts/include"] -- ghcconfig.h, ghcversion.h
        series | series < GHC_9_4 -> ["includes"] -- ghcconfig.h, MachDeps.h, MachRegs.h, CodeGen.Platform.hs
        _ -> error "ghcLibParserIncludeDirs: impossible case!",
      [hadrianGeneratedRoot ghcFlavor, stage0Compiler, "compiler"],
      ["compiler/utils" | ghcSeries ghcFlavor < GHC_8_10],
      ["libraries/containers/containers/include" | ghcSeries ghcFlavor >= GHC_9_8] -- containers.h
    ]

-- C-preprocessor "include dirs" for 'ghc-lib'.
ghcLibIncludeDirs :: GhcFlavor -> [FilePath]
ghcLibIncludeDirs = ghcLibParserIncludeDirs

-- Sort by length so the longest paths are at the front. We do this
-- so that in 'calcParserModules', longer substituions are performed
-- before shorter ones (and bad things will happen if that were
-- not the case).
sortDiffListByLength :: Set.Set FilePath -> Set.Set FilePath -> [FilePath]
sortDiffListByLength all excludes =
  nubOrd . sortOn (Down . length) $ Set.toList (Set.difference all excludes)

-- The "hs-source-dirs" universe.
allHsSrcDirs :: Bool -> GhcFlavor -> [Cabal] -> [FilePath]
allHsSrcDirs forDepends ghcFlavor lib =
  join
    [ [stage0Compiler],
      [dir | forDepends, dir <- [stage0Ghci, stage0GhcHeap]],
      [stage0GhcBoot | ghcSeries ghcFlavor >= GHC_8_10],
      map takeDirectory (cabalFileLibraries ghcFlavor),
      map (dropTrailingPathSeparator . normalise) (askFiles lib "hs-source-dirs:"),
      ["libraries/ghc-boot-th-internal"]
    ]

-- The "hs-source-dirs" for 'ghc-lib-parser'.
ghcLibParserHsSrcDirs :: Bool -> GhcFlavor -> [Cabal] -> [FilePath]
ghcLibParserHsSrcDirs forDepends ghcFlavor lib =
  let all = Set.fromList $ allHsSrcDirs forDepends ghcFlavor lib
      exclusions =
        case ghcSeries ghcFlavor of
          GHC_8_8 -> ["compiler/codeGen", "compiler/hieFile", "compile/llvmGen", "compiler/stranal", "compiler/rename", "compiler/stgSyn", "compiler/llvmGen"]
          GHC_8_10 -> ["compiler/nativeGen", "compiler/deSugar", "compiler/hieFile", "compiler/llvmGen", "compiler/stranal", "compiler/rename", "compiler/stgSyn"]
          _ -> []
   in sortDiffListByLength all $ Set.fromList [dir | not forDepends, dir <- exclusions]

-- The "hs-source-dirs" for 'ghc-lib'.
ghcLibHsSrcDirs :: Bool -> GhcFlavor -> [Cabal] -> [FilePath]
ghcLibHsSrcDirs forDepends ghcFlavor lib =
  let all = Set.fromList $ allHsSrcDirs forDepends ghcFlavor lib
      exclusions =
        case ghcSeries ghcFlavor of
          GHC_8_8 -> ["libraries/template-haskell", "libraries/ghc-boot-th", "compiler/basicTypes", "libraries/ghc-boot", "libraries/ghc-heap", "compiler/parser", "compiler/types"]
          GHC_8_10 -> ["ghc-lib/stage0/libraries/ghc-boot/build", "libraries/template-haskell", "libraries/ghc-boot-th", "compiler/basicTypes", "libraries/ghc-heap", "compiler/parser", "compiler/types"]
          GHC_9_0 -> ["ghc-lib/stage0/libraries/ghc-boot/build", "libraries/template-haskell", "libraries/ghc-boot-th", "libraries/ghc-heap"]
          GHC_9_2 -> ["ghc-lib/stage0/libraries/ghc-boot/build", "libraries/template-haskell", "libraries/ghc-boot-th", "libraries/ghc-heap"]
          GHC_9_4 -> ["ghc-lib/stage0/libraries/ghc-boot/build", "libraries/template-haskell", "libraries/ghc-boot-th", "libraries/ghc-heap", "libraries/ghci"]
          GHC_9_6 -> ["libraries/template-haskell", "libraries/ghc-boot-th", "libraries/ghc-boot", "libraries/ghc-heap", "libraries/ghci"]
          GHC_9_8 -> ["libraries/template-haskell", "libraries/ghc-boot-th", "libraries/ghc-boot", "libraries/ghc-heap", "libraries/ghc-platform/src", "libraries/ghc-platform"]
          GHC_9_10 -> ["libraries/template-haskell", "libraries/ghc-boot-th", "libraries/ghc-boot-th-internal", "libraries/ghc-boot", "libraries/ghc-heap", "libraries/ghc-platform/src", "libraries/ghc-platform", "libraries/ghci"]
          GHC_9_12 -> ["libraries/template-haskell", "libraries/ghc-boot-th", "libraries/ghc-boot-th-internal", "libraries/ghc-boot", "libraries/ghc-heap", "libraries/ghc-platform/src", "libraries/ghc-platform", "libraries/ghci", "libraries/ghc-internal/src"]
          GHC_9_14 -> ["libraries/template-haskell", "libraries/ghc-boot-th", "libraries/ghc-boot-th-internal", "libraries/ghc-boot", "", "libraries/ghc-heap", "libraries/ghc-platform/src", "libraries/ghc-platform", "libraries/ghci", "libraries/ghc-internal/src"]
   in sortDiffListByLength all $ Set.fromList [dir | not forDepends, dir <- exclusions]

-- File path constants.

dataDir :: FilePath
cabalFileBinary :: FilePath
ghcLibGeneratedPath :: FilePath
stage0Root, stage0Compiler, stage0Rts, stage0Libraries, stage0Lib :: FilePath
stage0GhcHeap, stage0GhcBoot, stage0Ghci :: FilePath
dataDir = stage0Lib

cabalFileBinary = "ghc/ghc-bin.cabal"

ghcLibGeneratedPath = "ghc-lib/generated"

stage0Root = "ghc-lib/stage0"

stage0Lib = stage0Root </> "lib"

stage0Libraries = stage0Root </> "libraries"

stage0Compiler = stage0Root </> "compiler/build"

stage0Rts = stage0Root </> "rts/build"

stage0GhcBoot = stage0Libraries </> "ghc-boot/build"

stage0GhcHeap = stage0Libraries </> "ghc-heap/build"

stage0Ghci = stage0Libraries </> "ghci/build"

-- Sources generated by Hadrian are written under this directory.
hadrianGeneratedRoot :: GhcFlavor -> FilePath
hadrianGeneratedRoot = \case
  f | f >= Ghc8101 -> stage0Lib
  _ -> ghcLibGeneratedPath

-- 'dataFiles' is a list of files to be installed for run-time use by
-- the package.
dataFiles :: GhcFlavor -> [FilePath]
dataFiles ghcFlavor =
  -- From ghc/ghc.mk: "The GHC programs need to depend on all the
  -- helper programs they might call and the settings files they
  -- use."
  join
    [ ["settings", "llvm-targets", "llvm-passes"],
      ["platformConstants" | ghcSeries ghcFlavor < GHC_9_2]
    ]

-- See 'hadrian/src/Rules/Generate.hs'.

cabalFileDependencies :: GhcFlavor -> [FilePath]
cabalFileDependencies ghcFlavor =
  [f | ghcSeries ghcFlavor > GHC_9_4, f <- cabalFileBinary : cabalFileLibraries ghcFlavor]

rtsDependencies :: GhcFlavor -> [FilePath]
rtsDependencies ghcFlavor =
  if ghcSeries ghcFlavor >= GHC_9_4
    then
      ((stage0Rts </> "include") </>) <$> ["ghcautoconf.h", "ghcplatform.h", "DerivedConstants.h"]
    else
      includesDependencies ghcFlavor ++ derivedConstantsDependencies ghcFlavor
  where
    includesDependencies :: GhcFlavor -> [FilePath]
    includesDependencies ghcFlavor =
      (hadrianGeneratedRoot ghcFlavor </>) <$> ["ghcautoconf.h", "ghcplatform.h", "ghcversion.h"]
    derivedConstantsDependencies :: GhcFlavor -> [FilePath]
    derivedConstantsDependencies ghcFlavor =
      (hadrianGeneratedRoot ghcFlavor </>) <$> ("DerivedConstants.h" : [x | ghcSeries ghcFlavor <= GHC_9_0, x <- ["GHCConstantsHaskellExports.hs", "GHCConstantsHaskellWrappers.hs", "GHCConstantsHaskellType.hs"]])

compilerDependencies :: GhcFlavor -> [FilePath]
compilerDependencies ghcFlavor =
  map (stage0Compiler </>) . join $
    [ ["primop-can-fail.hs-incl" | series < GHC_9_10],
      ["primop-code-size.hs-incl", "primop-commutable.hs-incl", "primop-data-decl.hs-incl", "primop-fixity.hs-incl"],
      [if series < GHC_9_10 then "primop-has-side-effects.hs-incl" else "primop-effects.hs-incl"],
      ["primop-list.hs-incl", "primop-out-of-line.hs-incl", "primop-primop-info.hs-incl", "primop-strictness.hs-incl", "primop-tag.hs-incl", "primop-vector-tycons.hs-incl", "primop-vector-tys-exports.hs-incl", "primop-vector-tys.hs-incl", "primop-vector-uniques.hs-incl"],
      ["primop-docs.hs-incl" | series >= GHC_9_0],
      [incl | series >= GHC_9_10, incl <- ["primop-is-work-free.hs-incl", "primop-is-cheap.hs-incl"]],
      ["primop-deprecations.hs-incl" | series > GHC_9_10],
      ["GHC/Platform/Constants.hs" | series >= GHC_9_2]
    ]
  where
    series = ghcSeries ghcFlavor

platformH :: GhcFlavor -> [FilePath]
platformH ghcFlavor =
  [stage0Compiler </> "ghc_boot_platform.h" | ghcSeries ghcFlavor < GHC_8_10]

packageCode :: GhcFlavor -> [FilePath]
packageCode ghcFlavor =
  join [[stage0Compiler </> "GHC/Settings/Config.hs" | ghcSeries ghcFlavor >= GHC_9_0], [stage0Compiler </> "Config.hs" | ghcSeries ghcFlavor < GHC_9_0], [stage0GhcBoot </> "GHC" </> x | ghcSeries ghcFlavor >= GHC_8_10, x <- ["Version.hs", "Platform/Host.hs"]]]

fingerprint :: GhcFlavor -> [FilePath]
fingerprint ghcFlavor =
  ["compiler/utils/Fingerprint.hsc" | ghcSeries ghcFlavor < GHC_8_10]

cHeaders :: GhcFlavor -> [String]
cHeaders ghcFlavor =
  join [[f | series > GHC_9_8, f <- ("rts/include/stg/MachRegs" </>) <$> ["arm32.h", "arm64.h", "loongarch64.h", "ppc.h", "riscv64.h", "s390x.h", "wasm32.h", "x86.h"]], ["libraries/containers/containers/include/containers.h" | series >= GHC_9_8], ["compiler" </> "ghc-llvm-version.h" | series >= GHC_9_4 && series <= GHC_9_8], [f | series >= GHC_9_4, f <- (("rts/include" </>) <$> ["ghcconfig.h", "ghcversion.h"]) ++ (("compiler" </>) <$> ["MachRegs.h", "CodeGen.Platform.h", "Bytecodes.h", "ClosureTypes.h", "FunTypes.h", "Unique.h"])], [f | series < GHC_9_4, f <- (("includes" </>) <$> ["MachDeps.h", "stg/MachRegs.h", "CodeGen.Platform.hs"]) ++ (("compiler" </>) <$> ["Unique.h", "HsVersions.h"])], [f | series < GHC_8_10, f <- ("compiler" </>) <$> ["nativeGen/NCG.h", "utils/md5.h"]]]
  where
    series = ghcSeries ghcFlavor

parsersAndLexers :: GhcFlavor -> [FilePath]
parsersAndLexers ghcFlavor =
  ("compiler" </>)
    <$> join [[x | ghcSeries ghcFlavor < GHC_9_0, x <- ("parser" </>) <$> ["Parser.y", "Lexer.x"]], [x | ghcSeries ghcFlavor >= GHC_9_0, x <- ("GHC" </>) <$> ["Parser.y", "Parser/Lexer.x"]], [x | ghcSeries ghcFlavor >= GHC_9_4, x <- ("GHC" </>) <$> ["Parser/HaddockLex.x", "Parser.hs-boot"]]]

ghcLibParserExtraFiles :: GhcFlavor -> [FilePath]
ghcLibParserExtraFiles ghcFlavor =
  join [cabalFileDependencies ghcFlavor, rtsDependencies ghcFlavor, compilerDependencies ghcFlavor, platformH ghcFlavor, fingerprint ghcFlavor, packageCode ghcFlavor, parsersAndLexers ghcFlavor, cHeaders ghcFlavor]

ghcLibExtraFiles :: GhcFlavor -> [FilePath]
ghcLibExtraFiles ghcFlavor =
  join [rtsDependencies ghcFlavor, compilerDependencies ghcFlavor, platformH ghcFlavor, fingerprint ghcFlavor, cHeaders ghcFlavor]

-- We generate some "placeholder" modules in `calcParserModues` They
-- get written here.
placeholderModulesDir :: FilePath
placeholderModulesDir = "placeholder_modules"

getGhcInfo :: IO [(String, String)]
getGhcInfo = do
  xs <- systemOutput_ "ghc --info"
  case reads xs of
    [(info, _)] -> return info
    _ -> error "failed to read output from `ghc info --info`"

ghcNumericVersion :: IO String
ghcNumericVersion = do
  ghcInfo <- getGhcInfo
  let ghcInfoMap = Map.fromList ghcInfo
  pure $ fromJust $ Map.lookup "Project version" ghcInfoMap

cabalPackageDb :: String -> IO String
cabalPackageDb ghcNumericVersion = do
  cabalStoreDir <- replace "\\" "\\\\" <$> systemOutput_ "cabal -v0 path --store-dir"
  ghcInfo <- getGhcInfo
  let ghcInfoMap = Map.fromList ghcInfo
  let ghcDir =
        case Map.lookup "Project Unit Id" ghcInfoMap of
          Just projectUnitId -> projectUnitId -- e.g. ghc-9.10.1-2e29
          Nothing -> "ghc-" ++ ghcNumericVersion
  pure $ cabalStoreDir ++ "/" ++ ghcDir ++ "/package.db"

setupModuleDepsPlaceholders :: GhcFlavor -> IO ()
setupModuleDepsPlaceholders _ = do
  forM_
    [ "compiler/",
      "libraries/ghc-heap/",
      "libraries/ghc-internal/",
      "libraries/ghci/"
    ]
    $ \path ->
      whenM (doesDirectoryExist path) $
        genPlaceholderModules path
  forM_
    [ "compiler/",
      "libraries/ghc-internal/src/"
    ]
    $ \path ->
      whenM (doesDirectoryExist path) $ do
        files <- filter ((`elem` [".hs-boot"]) . takeExtension) <$> listFilesRecursive path
        forM_ files $ \file -> do
          let p = fromJust (stripPrefix path file)
              new_p = placeholderModulesDir </> p
              dir = System.FilePath.takeDirectory new_p
          createDirectoryIfMissing True dir
          copyFile file new_p

calcModuleDeps :: [FilePath] -> [FilePath] -> [FilePath] -> GhcFlavor -> FilePath -> String -> String
calcModuleDeps includeDirs _hsSrcDirs hsSrcIncludes ghcFlavor cabalPackageDb ghcMakeModeOutputFile =
  unwords . join $
    [ ["ghc -M -dep-suffix '' -dep-makefile " ++ ghcMakeModeOutputFile],
      ["-clear-package-db -global-package-db -user-package-db -package-db " ++ cabalPackageDb],
      ["-package semaphore-compat" | series >= GHC_9_8],
#if __GLASGOW_HASKELL__ == 908 && __GLASGOW_HASKELL_PATCHLEVEL1__ == 4
      ["-hide-package os-string"], -- avoid System.OsString ambiguity with filepath-1.4.301.0
#endif
      ["-fno-safe-haskell" | series >= GHC_9_0], -- avoid warning: [GHC-98887] -XGeneralizedNewtypeDeriving is not allowed in Safe Haskell; ignoring -XGeneralizedNewtypeDeriving
      ["-DBIGNUM_NATIVE" | series > GHC_9_12],
      includeDirs,
      hsSrcIncludes,
      [placeholderModulesDir </> "Main.hs"]
    ]
  where
    series = ghcSeries ghcFlavor

readGhcMakeModeOutputFile :: FilePath -> [FilePath] -> IO [String]
readGhcMakeModeOutputFile file hsSrcDirs = do
  buf <- readFile' file
  -- The idea here is harvest from lines like
  -- 'compiler/prelude/PrelRules.o : compiler/prelude/PrelRules.hs',
  -- just the module name e.g. in this example, 'PrelRules'.
  -- Strip comment lines.
  let depends = filter (not . isPrefixOf "#") (lines buf)
      -- Restrict to Haskell source file lines.
      moduleLines = filter (isSuffixOf ".hs") depends
      -- Strip each line up-to and including ':'.
      modulePaths = map (trim . snd) (mapMaybe (stripInfix ":") moduleLines)
      -- Remove leading source directories from what's left.
      strippedModulePaths =
        foldl
          (\acc p -> map (replace (p ++ "/") "") acc)
          modulePaths
          hsSrcDirs
      -- Lastly, manipulate text like 'GHC/Exts/Heap/Constants.hs'
      -- into 'GHC.Exts.Heap.Constants'.
      modules = [replace "/" "." . dropSuffix ".hs" $ m | m <- strippedModulePaths, m /= "Main.hs"]

  return $ nubSort modules

calcParserModules :: GhcFlavor -> IO [String]
calcParserModules ghcFlavor = do
  let rootModulePath = placeholderModulesDir </> "Main.hs"
      series = ghcSeries ghcFlavor
      mainFile = "../ghc-lib-gen/ghc-lib-parser" </> show series </> "Main.hs"
  copyFile mainFile rootModulePath
  lib <- mapM readCabalFile (cabalFileLibraries ghcFlavor)
  cabalPackageDb <- cabalPackageDb =<< ghcNumericVersion
  let includeDirs = map ("-I" ++) (ghcLibParserIncludeDirs ghcFlavor)
      hsSrcDirs = placeholderModulesDir : ghcLibParserHsSrcDirs True ghcFlavor lib
      hsSrcIncludes = map ("-i" ++) hsSrcDirs
      cmd' = calcModuleDeps includeDirs hsSrcDirs hsSrcIncludes ghcFlavor cabalPackageDb ".parser-depends"
  putStrLn "# Generating 'ghc/.parser-depends'..."
  putStrLn $ "\n\n# Running: " ++ cmd'
  system_ cmd'
  readGhcMakeModeOutputFile ".parser-depends" hsSrcDirs

calcLibModules :: GhcFlavor -> IO [String]
calcLibModules ghcFlavor = do
  let rootModulePath = placeholderModulesDir </> "Main.hs"
      series = ghcSeries ghcFlavor
      mainFile = "../ghc-lib-gen/ghc-lib" </> show series </> "Main.hs"
  copyFile mainFile rootModulePath
  lib <- mapM readCabalFile (cabalFileLibraries ghcFlavor)
  cabalPackageDb <- cabalPackageDb =<< ghcNumericVersion
  let includeDirs = map ("-I" ++) (ghcLibIncludeDirs ghcFlavor)
      hsSrcDirs = placeholderModulesDir : ghcLibHsSrcDirs True ghcFlavor lib
      hsSrcIncludes = map ("-i" ++) hsSrcDirs
      cmd' = calcModuleDeps includeDirs hsSrcDirs hsSrcIncludes ghcFlavor cabalPackageDb ".lib-depends"
  putStrLn "# Generating 'ghc/.lib-depends'..."
  putStrLn $ "\n\n# Running: " ++ cmd'
  system_ cmd'
  readGhcMakeModeOutputFile ".lib-depends" hsSrcDirs

applyPatchGhcInternalEventWindowsHsc :: GhcFlavor -> IO ()
applyPatchGhcInternalEventWindowsHsc ghcFlavor = do
  let series = ghcSeries ghcFlavor
  when (series > GHC_9_10) $ do
    writeFile "libraries/ghc-internal/src/GHC/Internal/Event/Windows.hsc"
      . replace
        ( unlines
            [ "#if defined(DEBUG_TRACE)",
              "import {-# SOURCE #-} GHC.Internal.Debug.Trace (traceEventIO)",
              "#endif"
            ]
        )
        ""
      . replace
        ( unlines
            [ "#if defined(DEBUG)",
              "import GHC.Internal.Foreign.C",
              "import GHC.Internal.System.Posix.Internals (c_write)",
              "import GHC.Internal.Conc.Sync (myThreadId)",
              "#endif"
            ]
        )
        ""
      =<< readFile' "libraries/ghc-internal/src/GHC/Internal/Event/Windows.hsc"

applyPatchTemplateHaskellLanguageHaskellTHSyntax :: GhcFlavor -> IO ()
applyPatchTemplateHaskellLanguageHaskellTHSyntax ghcFlavor = do
  -- Revert the changes that result from the commit:
  --   https://gitlab.haskell.org/ghc/ghc/-/commit/983ce55815f2dd57f84ee86eee97febf7d80b470
  -- The above commit uses `TemplateHaskellQuotes` to define `Name`s.
  -- Doing that produces values of type `Name` from the
  -- template-haskell package rather than values of type `Name` as
  -- defined in `Language.Haskell.TH.Syntax`.
  let series = ghcSeries ghcFlavor
  when (series >= GHC_9_8 && series <= GHC_9_10) $ do
    writeFile "libraries/template-haskell/Language/Haskell/TH/Syntax.hs"
      . replace
        "{-# LANGUAGE TemplateHaskellQuotes #-}"
        ""
      . replace
        "TYPE, RuntimeRep(..), Multiplicity (..) )"
        "TYPE, RuntimeRep(..) )"
      . replace
        "import Foreign.C.Types"
        (unlines ["import Foreign.C.Types", "import GHC.Stack"])
      . replace
        "mkFixedName = 'Fixed.MkFixed"
        "mkFixedName = mkNameG DataName \"base\" \"Data.Fixed\" \"MkFixed\""
      . replace
        "addrToByteArrayName = 'addrToByteArray"
        ( unlines
            [ "addrToByteArrayName = helper",
              "  where",
              "    helper :: HasCallStack => Name",
              "    helper =",
              "      case getCallStack ?callStack of",
              "        [] -> error \"addrToByteArrayName: empty call stack\"",
              "        (_, SrcLoc{..}) : _ -> mkNameG_v srcLocPackage srcLocModule \"addrToByteArray\""
            ]
        )
      . replace
        (unlines ["trueName  = 'True", "falseName = 'False"])
        (unlines ["trueName  = mkNameG DataName \"ghc-prim\" \"GHC.Types\" \"True\"", "falseName = mkNameG DataName \"ghc-prim\" \"GHC.Types\" \"False\""])
      . replace
        (unlines ["nothingName = 'Nothing", "justName    = 'Just"])
        (unlines ["nothingName = mkNameG DataName \"base\" \"GHC.Maybe\" \"Nothing\"", "justName    = mkNameG DataName \"base\" \"GHC.Maybe\" \"Just\""])
      . replace
        (unlines ["leftName  = 'Left", "rightName = 'Right"])
        (unlines ["leftName  = mkNameG DataName \"base\" \"Data.Either\" \"Left\"", "rightName = mkNameG DataName \"base\" \"Data.Either\" \"Right\""])
      . replace
        "nonemptyName = '(:|)"
        "nonemptyName = mkNameG DataName \"base\" \"GHC.Base\" \":|\""
      . replace
        (unlines ["oneName  = 'One", "manyName = 'Many"])
        (unlines ["oneName  = mkNameG DataName \"ghc-prim\" \"GHC.Types\" \"One\"", "manyName = mkNameG DataName \"ghc-prim\" \"GHC.Types\" \"Many\""])
      =<< readFile' "libraries/template-haskell/Language/Haskell/TH/Syntax.hs"

applyPatchTemplateHaskellCabal :: GhcFlavor -> IO ()
applyPatchTemplateHaskellCabal ghcFlavor = do
  when (series == GHC_9_4) $ do
    -- In
    -- https://gitlab.haskell.org/ghc/ghc/-/commit/b151b65ec469405dcf25f9358e7e99bcc8c2b3ac
    -- (2022/7/05) a temporary change is made to provide for vendoring
    -- filepath inside template-haskell. This breaks our simple cabal
    -- parsing so workaround while this situation exists.
    writeFile "libraries/template-haskell/template-haskell.cabal.in"
      . replace
        ( unlines
            [ "    if flag(vendor-filepath)",
              "      other-modules:",
              "        System.FilePath",
              "        System.FilePath.Posix",
              "        System.FilePath.Windows",
              "      hs-source-dirs: ../filepath .",
              "      default-extensions:",
              "        ImplicitPrelude",
              "    else",
              "      build-depends: filepath",
              "      hs-source-dirs: ."
            ]
        )
        "        filepath"
      . replace
        ( unlines
            [ "    if flag(vendor-filepath)",
              "      other-modules:",
              "        System.FilePath",
              "        System.FilePath.Posix",
              "        System.FilePath.Windows",
              "      hs-source-dirs: ./vendored-filepath .",
              "      default-extensions:",
              "        ImplicitPrelude",
              "    else",
              "      build-depends: filepath",
              "      hs-source-dirs: ."
            ]
        )
        "        filepath"
      =<< readFile' "libraries/template-haskell/template-haskell.cabal.in"

  when (series > GHC_9_10) $ do
    writeFile "libraries/template-haskell/template-haskell.cabal.in"
      . replace
        ( unlines
            [ "    other-modules:",
              "      System.FilePath",
              "      System.FilePath.Posix",
              "      System.FilePath.Windows",
              "    hs-source-dirs: ./vendored-filepath .",
              "    default-extensions:",
              "      ImplicitPrelude"
            ]
        )
        ( unlines
            [ "    build-depends:",
              "      filepath",
              "    hs-source-dirs: ."
            ]
        )
      =<< readFile' "libraries/template-haskell/template-haskell.cabal.in"

  when (series >= GHC_9_6 && series <= GHC_9_10) $ do
    -- As of
    -- https://gitlab.haskell.org/ghc/ghc/-/commit/9034fadaf641c3821db6e066faaf1a62ed236c13
    -- GHC always relies on vendored filepath sources
    -- `System.FilePath`, `System.FilePath.Posix`,
    -- `System.FilePath.Posix`. If we add these modules into
    -- ghc-lib-parser we run into ambiguity errors later in the hlint
    -- stack (i.e. ghc-lib-parser + filepath = ambiguity).
    -- Fortunately, it seems we can continue to get away with what we
    -- we've been doing up to now and simply say in ghc-lib-parser
    -- cabal that it `build-depends` on filepath.
    writeFile "libraries/template-haskell/template-haskell.cabal.in"
      . replace
        ( unlines
            [ "    other-modules:",
              "      System.FilePath",
              "      System.FilePath.Posix",
              "      System.FilePath.Windows",
              "    hs-source-dirs: ./vendored-filepath ."
            ]
        )
        ( unlines
            [ "    build-depends:",
              "        filepath",
              "    hs-source-dirs: ."
            ]
        )
      =<< readFile' "libraries/template-haskell/template-haskell.cabal.in"
  where
    series = ghcSeries ghcFlavor

-- Avoid duplicate symbols with HSghc-heap (see issue
-- https://github.com/digital-asset/ghc-lib/issues/210).
applyPatchHeapClosures :: GhcFlavor -> IO ()
applyPatchHeapClosures _ = do
  writeFile "libraries/ghc-heap/cbits/HeapPrim.cmm"
    . replace
      "aToWordzh"
      "Ghclib_aToWordzh"
    . replace
      "reallyUnsafePtrEqualityUpToTag"
      "Ghclib_reallyUnsafePtrEqualityUpToTag"
    =<< readFile' "libraries/ghc-heap/cbits/HeapPrim.cmm"
  writeFile "libraries/ghc-heap/GHC/Exts/Heap/Closures.hs"
    . replace
      "\"aToWordzh\""
      "\"Ghclib_aToWordzh\""
    . replace
      "\"reallyUnsafePtrEqualityUpToTag\""
      "\"Ghclib_reallyUnsafePtrEqualityUpToTag\""
    =<< readFile' "libraries/ghc-heap/GHC/Exts/Heap/Closures.hs"

-- Rename a file then replace occurences of the name of the file in
-- sources.
renameFileRewriteSrcs :: FilePath -> FilePath -> [FilePath] -> [String] -> IO ()
renameFileRewriteSrcs root f dirs exts = do
  let (old, new) = (f, "Ghclib" ++ f)
  renameFile (root </> old) (root </> new)
  forM_ dirs $ \dir -> do
    files <- filter ((`elem` exts) . takeExtension) <$> listFilesRecursive dir
    forM_ files $ \file -> writeFile file . replace old new =<< readFile' file

-- Rename 'HsVersions.h' to 'GhclibHsVersions.h' (see
-- https://github.com/digital-asset/ghc-lib/issues/204).
applyPatchHsVersions :: GhcFlavor -> IO ()
applyPatchHsVersions ghcFlavor =
  when (ghcSeries ghcFlavor < GHC_9_4) $ do
    renameFileRewriteSrcs "compiler" "HsVersions.h" ["compiler", stage0Compiler] [".hs", ".y", ".hsc"]

-- Rename 'DerivedConstants.h' to 'GhclibDerivedConstants.h'.
applyPatchDerivedConstants :: GhcFlavor -> IO ()
applyPatchDerivedConstants ghcFlavor =
  renameFileRewriteSrcs
    ( if ghcSeries ghcFlavor >= GHC_9_4
        then
          stage0Rts </> "include"
        else
          hadrianGeneratedRoot ghcFlavor
    )
    "DerivedConstants.h"
    (["compiler", stage0Compiler] ++ [stage0Rts | ghcSeries ghcFlavor >= GHC_9_4])
    [".hs", ".y", ".hsc"]

-- Selectively disable optimizations in some particular files so as
-- to reduce (user) compile times. The files we apply this to were
-- those identified as bottlenecks in the 2019 GSoC Hadrian speed
-- project.
applyPatchDisableCompileTimeOptimizations :: GhcFlavor -> IO ()
applyPatchDisableCompileTimeOptimizations ghcFlavor =
  let files
        | ghcSeries ghcFlavor >= GHC_9_0 = ["compiler/GHC/Driver/Session.hs", "compiler/GHC/Hs.hs"]
        | ghcSeries ghcFlavor >= GHC_8_10 = ["compiler/main/DynFlags.hs", "compiler/GHC/Hs.hs"]
        | otherwise = ["compiler/main/DynFlags.hs", "compiler/hsSyn/HsInstances.hs"]
   in forM_ files $
        \file ->
          writeFile file
            . ("{-# OPTIONS_GHC -O0 #-}\n" ++)
            =<< readFile' file

applyPatchGHCiInfoTable :: GhcFlavor -> IO ()
applyPatchGHCiInfoTable ghcFlavor = do
  when (ghcFlavor == DaGhc881) $
    -- Drop references to RTS symbols in GHCi so we can build with GHC
    -- 9. These functions are never used since GHCi doesn’t work in
    -- ghc-lib anyway.
    writeFile infoTableHsc
      . replace
        ( unlines
            [ "foreign import ccall unsafe \"allocateExec\"",
              "  _allocateExec :: CUInt -> Ptr (Ptr a) -> IO (Ptr a)",
              "",
              "foreign import ccall unsafe \"flushExec\"",
              "  _flushExec :: CUInt -> Ptr a -> IO ()"
            ]
        )
        ( unlines
            [ "_allocateExec :: CUInt -> Ptr (Ptr a) -> IO (Ptr a)",
              "_allocateExec = error \"_allocateExec stub for ghc-lib\"",
              "",
              "_flushExec :: CUInt -> Ptr a -> IO ()",
              "_flushExec = error \"_flushExec stub for ghc-lib\""
            ]
        )
      =<< readFile' infoTableHsc
  when (ghcSeries ghcFlavor >= GHC_9_2) $ do
    writeFile infoTableHsc
      . replace
        (unlines newExecConItblBefore)
        ( "#if MIN_VERSION_rts(1,0,1)\n"
            <> unlines newExecConItblBefore
            <> "#else\n"
            <> unlines newExecConItblAfter
            <> "#endif\n"
        )
      . replace
        "fillExecBuffer :: CSize -> (Ptr a -> Ptr a -> IO ()) -> IO (Ptr a)\n"
        ( "#if MIN_VERSION_rts(1,0,1)\n"
            <> "fillExecBuffer :: CSize -> (Ptr a -> Ptr a -> IO ()) -> IO (Ptr a)\n"
            <> "#endif\n"
        )
      . replace
        ( if ghcSeries ghcFlavor >= GHC_9_4
            then
              "#error Sorry, rts versions <= 1.0 are not supported"
            else
              "#error hi"
        )
        ( unlines
            [ "foreign import ccall unsafe \"allocateExec\"",
              "  _allocateExec :: CUInt -> Ptr (Ptr a) -> IO (Ptr a)",
              "",
              "foreign import ccall unsafe \"flushExec\"",
              "  _flushExec :: CUInt -> Ptr a -> IO ()"
            ]
        )
      =<< readFile' infoTableHsc
  where
    infoTableHsc = "libraries/ghci/GHCi/InfoTable.hsc"

    newExecConItblBefore =
      [ "newExecConItbl :: Bool -> StgInfoTable -> ByteString -> IO (FunPtr ())",
        "newExecConItbl tables_next_to_code obj con_desc = do",
        "    sz0 <- sizeOfEntryCode tables_next_to_code",
        "    let lcon_desc = BS.length con_desc + 1{- null terminator -}",
        "        -- SCARY",
        "        -- This size represents the number of bytes in an StgConInfoTable.",
        "        sz = fromIntegral $ conInfoTableSizeB + sz0",
        "            -- Note: we need to allocate the conDesc string next to the info",
        "            -- table, because on a 64-bit platform we reference this string",
        "            -- with a 32-bit offset relative to the info table, so if we",
        "            -- allocated the string separately it might be out of range.",
        "",
        "    ex_ptr <- fillExecBuffer (sz + fromIntegral lcon_desc) $ \\wr_ptr ex_ptr -> do",
        "        let cinfo = StgConInfoTable { conDesc = ex_ptr `plusPtr` fromIntegral sz",
        "                                    , infoTable = obj }",
        "        pokeConItbl tables_next_to_code wr_ptr ex_ptr cinfo",
        "        BS.useAsCStringLen con_desc $ \\(src, len) ->",
        "            copyBytes (castPtr wr_ptr `plusPtr` fromIntegral sz) src len",
        "        let null_off = fromIntegral sz + fromIntegral (BS.length con_desc)",
        "        poke (castPtr wr_ptr `plusPtr` null_off) (0 :: Word8)",
        "",
        "    pure $ if tables_next_to_code",
        "      then castPtrToFunPtr $ ex_ptr `plusPtr` conInfoTableSizeB",
        "      else castPtrToFunPtr ex_ptr"
      ]

    newExecConItblAfter =
      [ "newExecConItbl :: Bool -> StgInfoTable -> ByteString -> IO (FunPtr ())",
        "newExecConItbl tables_next_to_code obj con_desc",
        "   = alloca $ \\pcode -> do",
        "        sz0 <- sizeOfEntryCode tables_next_to_code",
        "        let lcon_desc = BS.length con_desc + 1{- null terminator -}",
        "            -- SCARY",
        "            -- This size represents the number of bytes in an StgConInfoTable.",
        "            sz = fromIntegral $ conInfoTableSizeB + sz0",
        "               -- Note: we need to allocate the conDesc string next to the info",
        "               -- table, because on a 64-bit platform we reference this string",
        "               -- with a 32-bit offset relative to the info table, so if we",
        "               -- allocated the string separately it might be out of range.",
        "        wr_ptr <- _allocateExec (sz + fromIntegral lcon_desc) pcode",
        "        ex_ptr <- peek pcode",
        "        let cinfo = StgConInfoTable { conDesc = ex_ptr `plusPtr` fromIntegral sz",
        "                                    , infoTable = obj }",
        "        pokeConItbl tables_next_to_code wr_ptr ex_ptr cinfo",
        "        BS.useAsCStringLen con_desc $ \\(src, len) ->",
        "            copyBytes (castPtr wr_ptr `plusPtr` fromIntegral sz) src len",
        "        let null_off = fromIntegral sz + fromIntegral (BS.length con_desc)",
        "        poke (castPtr wr_ptr `plusPtr` null_off) (0 :: Word8)",
        "        _flushExec sz ex_ptr -- Cache flush (if needed)",
        "        pure $ if tables_next_to_code",
        "          then castPtrToFunPtr $ ex_ptr `plusPtr` conInfoTableSizeB",
        "          else castPtrToFunPtr ex_ptr"
      ]

applyPatchGHCiMessage :: GhcFlavor -> IO ()
applyPatchGHCiMessage ghcFlavor =
  when (ghcSeries ghcFlavor >= GHC_9_2) $ do
    -- Synthesize a definition of MIN_VERSION_ghc_heap. If X.Y.Z is
    -- the current version, then MIN_VERSION_ghc_heap(a, b, c) is a
    -- test of whether X.Y.Z >= a.b.c (that is, ghc-heap X.Y.Z is at
    -- least a.b.c).
    versionText <-
      if ghcSeries ghcFlavor < GHC_9_4
        then
          readFile' (hadrianGeneratedRoot ghcFlavor </> "ghcversion.h")
        else
          readFile' "rts/include/ghcversion.h" -- now generated by ./configure
    let ls = lines versionText
        Just version = firstJust (stripPrefix "#define __GLASGOW_HASKELL__ ") ls
        Just patchLevel = firstJust (stripPrefix "#define __GLASGOW_HASKELL_PATCHLEVEL1__ ") ls
        major1Major2 = read @Int version
        minor = read @Int patchLevel
        [x, y, z] = map show [major1Major2 `div` 100, major1Major2 `mod` 100, minor]
        rs =
          [ "#ifndef MIN_VERSION_ghc_heap",
            "#define MIN_VERSION_ghc_heap(major1,major2,minor) (\\",
            "  (major1) <  " ++ x ++ " || \\",
            "  (major1) == " ++ x ++ " && (major2) <  " ++ y ++ " || \\",
            "  (major1) == " ++ x ++ " && (major2) == " ++ y ++ " && (minor) <= " ++ z ++ ")",
            "#endif /* MIN_VERSION_ghc_heap */"
          ]
    -- Write this definition before it's tested on.
    writeFile messageHs
      . replace
        "#if MIN_VERSION_ghc_heap(8,11,0)"
        (unlines rs <> "#if MIN_VERSION_ghc_heap(8,11,0)")
      =<< readFile' messageHs
  where
    messageHs = "libraries/ghci/GHCi/Message.hs"

applyPatchHaddockHs :: GhcFlavor -> IO ()
applyPatchHaddockHs ghcFlavor = do
  -- See https://github.com/ndmitchell/hlint/issues/1224
  when
    (ghcFlavor `elem` [Ghc901, Ghc902])
    ( writeFile haddockHs
        . replace
          "-- *"
          "-- -"
        =<< readFile' haddockHs
    )
  -- See https://github.com/digital-asset/ghc-lib/issues/344
  when
    (ghcSeries ghcFlavor == GHC_9_2)
    ( writeFile ffiClosuresHs
        . replace
          "-- *"
          "-- -"
        =<< readFile' ffiClosuresHs
    )

  -- See https://github.com/digital-asset/ghc-lib/issues/391
  when
    (ghcFlavor `elem` [Ghc923, Ghc924, Ghc925, Ghc926, Ghc927, Ghc928])
    ( writeFile codeGenHs . replace "{- | debugIsOn -}" ""
        =<< readFile' codeGenHs
    )
  where
    haddockHs = "compiler/GHC/Parser/PostProcess/Haddock.hs"
    ffiClosuresHs = "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures.hs"
    codeGenHs = "compiler/GHC/CmmToAsm/AArch64/CodeGen.hs"

-- Support for unboxed tuples got landed 03/20/2021
-- (https://gitlab.haskell.org/ghc/ghc/-/commit/1f94e0f7601f8e22fdd81a47f130650265a44196#4ec156a7b95e9c7a690c99bc79e6e0edf60a51dc)
-- Older versions of the rts don't define two of the numeric
-- instruction codes that this support relies on.
applyPatchRtsBytecodes :: GhcFlavor -> IO ()
applyPatchRtsBytecodes ghcFlavor = do
  let series = ghcSeries ghcFlavor
  when
    (series >= GHC_9_2 && series < GHC_9_6)
    ( writeFile asmHs
        . replace
          "#include \"rts/Bytecodes.h\""
          ( unlines
              [ "#include \"rts/Bytecodes.h\"",
                "#if __GLASGOW_HASKELL__ <= 901",
                "#  define bci_RETURN_T          69",
                "#  define bci_PUSH_ALTS_T       70",
                "#endif"
              ]
          )
        =<< readFile' asmHs
    )
  where
    asmHs = "compiler/GHC/ByteCode/Asm.hs"

-- Workaround lack of newer ghc-prim 12/3/2019
-- (https://gitlab.haskell.org/ghc/ghc/commit/705a16df02411ec2445c9a254396a93cabe559ef)
applyPatchGhcPrim :: GhcFlavor -> IO ()
applyPatchGhcPrim ghcFlavor = do
  let series = ghcSeries ghcFlavor
  when (series >= GHC_9_0 && series < GHC_9_6) $ do
    let tysPrim = "compiler/GHC/Builtin/Types/Prim.hs"
    writeFile tysPrim
      . replaceIfGhcPrim070Else
        0
        "bcoPrimTyCon = pcPrimTyCon0 bcoPrimTyConName LiftedRep"
        "bcoPrimTyCon = pcPrimTyCon0 bcoPrimTyConName UnliftedRep"
      . replaceIfGhcPrim070Else
        0
        "bcoPrimTyConName              = mkPrimTc (fsLit \"BCO\") bcoPrimTyConKey bcoPrimTyCon"
        "bcoPrimTyConName              = mkPrimTc (fsLit \"BCO#\") bcoPrimTyConKey bcoPrimTyCon"
      =<< readFile' tysPrim
    let createBCO = "libraries/ghci/GHCi/CreateBCO.hs"
    writeFile createBCO
      . replace
        "{-# LANGUAGE RecordWildCards #-}"
        "{-# LANGUAGE RecordWildCards #-}\n{-# LANGUAGE CPP #-}"
      . replaceIfGhcPrim070Else
        5
        "do linked_bco <- linkBCO' arr bco"
        "do BCO bco# <- linkBCO' arr bco"
      . replaceIfGhcPrim070Else
        11
        "then return (HValue (unsafeCoerce linked_bco))\n           else case mkApUpd0# linked_bco of { (# final_bco #) ->"
        "then return (HValue (unsafeCoerce# bco#))\n           else case mkApUpd0# bco# of { (# final_bco #) ->"
      . replaceIfGhcPrim070Else
        6
        "bco <- linkBCO' arr bco\n      writePtrsArrayBCO i bco marr"
        "BCO bco# <- linkBCO' arr bco\n      writePtrsArrayBCO i bco# marr"
      . replaceIfGhcPrim070Else
        0
        "writePtrsArrayBCO :: Int -> BCO -> PtrsArr -> IO ()"
        "writePtrsArrayBCO :: Int -> BCO# -> PtrsArr -> IO ()"
      . replaceIfGhcPrim070Else
        0
        "writePtrsArrayMBA :: Int -> MutableByteArray# s -> PtrsArr -> IO ()"
        "data BCO = BCO BCO#\nwritePtrsArrayMBA :: Int -> MutableByteArray# s -> PtrsArr -> IO ()"
      . replaceIfGhcPrim070Else
        2
        "newBCO# instrs lits ptrs arity bitmap s"
        "case newBCO# instrs lits ptrs arity bitmap s of\n    (# s1, bco #) -> (# s1, BCO bco #)"
      =<< readFile' createBCO
  where
    replaceIfGhcPrim070Else :: Int -> String -> String -> String -> String
    replaceIfGhcPrim070Else n s r = replace s (ifGhcPrim070Else n s r)

    ifGhcPrim070Else :: Int -> String -> String -> String
    ifGhcPrim070Else n s r =
      let indent n s = replicate n ' ' ++ s
       in unlines ["\n#if MIN_VERSION_ghc_prim(0, 7, 0)", indent n s, "#else", indent n r, "#endif"]

-- Fix up these rts include paths. We don't ship rts headers since we
-- run ghc-lib using the RTS of the compiler we build with - we go to
-- the compiler installation for those.
applyPatchRtsIncludePaths :: GhcFlavor -> IO ()
applyPatchRtsIncludePaths ghcFlavor = do
  let files =
        ["compiler/GHC/Runtime/Heap/Layout.hs" | ghcSeries ghcFlavor >= GHC_9_0]
          ++ ["compiler/cmm/SMRep.hs" | ghcSeries ghcFlavor < GHC_9_0]
          ++ ["compiler/GHC/StgToCmm/Layout.hs" | ghcSeries ghcFlavor >= GHC_8_10]
          ++ ["compiler/codeGen/StgCmmLayout.hs" | ghcSeries ghcFlavor < GHC_8_10]
  forM_ files $
    \file ->
      writeFile file
        . replace
          "../includes/rts"
          "rts"
        =<< readFile' file

-- Mangle exported C symbols to avoid collisions between the symbols
-- in ghc-lib-parser and ghc.
mangleCSymbols :: GhcFlavor -> IO ()
mangleCSymbols ghcFlavor = do
  let ghcLibParserPrefix = "ghc_lib_parser_"
  let prefixSymbol s = replace s (ghcLibParserPrefix <> s)
  let prefixForeignImport s =
        replace
          ("foreign import ccall unsafe " <> show s)
          ("foreign import ccall unsafe " <> show (ghcLibParserPrefix <> s))
  let genSym = "genSym"
  let initGenSym = "initGenSym"
  let enableTimingStats = "enableTimingStats"
  let setHeapSize = "setHeapSize"
  let keepCAFsForGHCi = "keepCAFsForGHCi"
  keepCAFsForCHCiExists <- doesFileExist "compiler/cbits/keepCAFsForGHCi.c"
  when keepCAFsForCHCiExists $ do
    let file = "compiler/cbits/keepCAFsForGHCi.c"
     in writeFile file
          . prefixSymbol keepCAFsForGHCi
          =<< readFile' file
    let file = "compiler/GHC.hs"
     in writeFile file
          . prefixForeignImport keepCAFsForGHCi
          =<< readFile' file
  let file = "compiler/cbits/genSym.c"
   in writeFile file
        . prefixSymbol genSym
        . prefixSymbol initGenSym
        . replace "#if !MIN_VERSION_GLASGOW_HASKELL(9,9,0,0)" "#if !MIN_VERSION_GLASGOW_HASKELL(9,8,4,0)"
        =<< readFile' file
  when (ghcFlavor == Ghc984) $
    let file = "compiler/cbits/genSym.c"
     in writeFile file
          . replace
            "HsWord64 u = atomic_inc64"
            "HsWord64 u = atomic_inc"
          =<< readFile' file
  let files
        | ghcSeries ghcFlavor >= GHC_9_0 = map ("compiler/GHC/Types" </>) ["Unique/Supply.hs", "Unique.hs"]
        | otherwise = ["compiler/basicTypes/UniqSupply.hs"]
  forM_ files $ \file ->
    writeFile file
      . prefixForeignImport genSym
      . prefixForeignImport initGenSym
      =<< readFile' file
  let cUtils
        | ghcSeries ghcFlavor >= GHC_9_0 = ["compiler/cbits/cutils.c"]
        | otherwise = ["compiler/parser/cutils.c", "compiler/parser/cutils.h"]
  forM_ cUtils $ \file ->
    writeFile file
      . prefixSymbol enableTimingStats
      . prefixSymbol setHeapSize
      =<< readFile' file
  let file
        | ghcSeries ghcFlavor >= GHC_9_0 = "compiler/GHC/Driver/Session.hs"
        | otherwise = "compiler/main/DynFlags.hs"
   in writeFile file
        . prefixForeignImport enableTimingStats
        . prefixForeignImport setHeapSize
        =<< readFile' file

-- Setting DSTAGE=2 will cause GHC to use getOrSetLibHSghc in
-- FastString, DynFlags and Linker so we patch away that usage while
-- leaving -DSTAGE=2 on since it is useful in other places, e.g.,
-- MachDeps.h.
--
-- See https://github.com/ndmitchell/hlint/issues/637 for an issue caused
-- by using getOrSetLibHSghc for the FastString table.
applyPatchStage :: GhcFlavor -> IO ()
applyPatchStage ghcFlavor =
  -- On master, `ghcplatform.h` sets `GHC_STAGE` to `1` and we no
  -- longer are required to pass `-DGHC_STAGE=2` to `cpp-options` to
  -- get a build (`MachDeps.h` does not hide its contents from stages
  -- below 2 anymore). All usages of `getOrSetLibHSghc*` require
  -- `GHC_STAGE >= 2`. Thus, it's no longer neccessary to patch here.
  when (ghcSeries ghcFlavor < GHC_8_10) $
    forM_ ["compiler/ghci/Linker.hs", "compiler/utils/FastString.hs", "compiler/main/DynFlags.hs"] $
      \file ->
        (writeFile file . replace "STAGE >= 2" "0" . replace "STAGE < 2" "1")
          =<< readFile' file

applyPatchAclocal :: GhcFlavor -> IO ()
applyPatchAclocal ghcFlavor =
  when (ghcFlavor <= Ghc901) $
    writeFile aclocalm4
      . replace "_AC_PROG_CC_C99" "AC_PROG_CC_C99"
      . replace "\"$AlexCmd\" -v" "\"$AlexCmd\" -V"
      . replace "if ! \"$CXX\"" "if ! eval \"$CXX\""
      =<< readFile' aclocalm4
  where
    aclocalm4 = "aclocal.m4"

applyPatchFptoolsAlex :: GhcFlavor -> IO ()
applyPatchFptoolsAlex ghcFlavor = do
  fptools_alex_exists <- doesFileExist fptools_alex_m4
  when (fptools_alex_exists && ghcFlavor <= Ghc982) $
    writeFile fptools_alex_m4
      . replace "\"$AlexCmd\" -v" "\"$AlexCmd\" -V"
      =<< readFile' fptools_alex_m4
  where
    fptools_alex_m4 = "m4/fptools_alex.m4"

applyPatchFpFindCxxStdLib :: GhcFlavor -> IO ()
applyPatchFpFindCxxStdLib ghcFlavor = do
  fp_find_cxx_std_lib_exists <- doesFileExist fp_find_cxx_std_lib_m4
  when (fp_find_cxx_std_lib_exists && ghcFlavor <= Ghc982) $
    writeFile fp_find_cxx_std_lib_m4
      . replace "if ! \"$CXX\"" "if ! eval \"$CXX\""
      =<< readFile' fp_find_cxx_std_lib_m4
  where
    fp_find_cxx_std_lib_m4 = "m4/fp_find_cxx_std_lib.m4"

{- The MonoLocalBinds extension in ghc-cabal.in was default enabled
   02-Sep-2020 in commit
   https://gitlab.haskell.org/ghc/ghc/-/commit/bfab2a30be5cc68e7914c3f6bb9ae4ad33283ffc.
   The files (of the ghc-heap boot library):
     - InfoTable.hsc
     - InfoTableProf.hsc
   rely on this extension not being enabled.
-}
applyPatchNoMonoLocalBinds :: GhcFlavor -> IO ()
applyPatchNoMonoLocalBinds _ =
  forM_ ["libraries/ghc-heap/GHC/Exts/Heap/InfoTable.hsc", "libraries/ghc-heap/GHC/Exts/Heap/InfoTableProf.hsc"] $
    \file ->
      (writeFile file . ("{-# LANGUAGE NoMonoLocalBinds #-}\n" ++))
        =<< readFile' file

{- 'CmmParse.y' on the ghc-8.10.* branches is missing an import. It's
 unclear why the stack ghc-lib build succeeds (and hence CI) but it
 certainly does not build with cabal directly -- see
 https://github.com/digital-asset/ghc-lib/issues/243 for where the
 problem was first reported. On master, this file has moved to
 GHC/cmm/Parser.y and already contains the missing directive so
 nothing to do there.

Update 30th Dec, 2020: This appears to be more to do with the compiler
in play than the flavor. It's still mysterious but not interesting
enough to dig into further. Just do the rewrite unconditionally.
 -}
applyPatchCmmParseNoImplicitPrelude :: GhcFlavor -> IO ()
applyPatchCmmParseNoImplicitPrelude _ = do
  let cmmParse = "compiler/cmm/CmmParse.y"
  fileExists <- doesFileExist cmmParse
  when fileExists $
    writeFile cmmParse
      . replace
        "import GhcPrelude"
        "import GhcPrelude\nimport qualified Prelude"
      =<< readFile' cmmParse

applyPatchHadrianCabalProject :: GhcFlavor -> IO ()
applyPatchHadrianCabalProject _ = do
  cabalProjectContents <- lines' <$> readFile' cabalProject
  cabalProjectContents <- pure (unlines (cabalProjectContents ++ ["flags:-selftest -with_bazel"]))
  writeFile cabalProject cabalProjectContents
  whenM (doesPathExist cabalProjectFreeze) $ removePathForcibly cabalProjectFreeze
  where
    lines' s = [l | l <- lines s, not $ "index-state" `isPrefixOf` l]
    cabalProject = "hadrian" </> "cabal.project"
    cabalProjectFreeze = cabalProject ++ ".freeze"

applyPatchCompilerGHCParserLexer :: GhcFlavor -> IO ()
applyPatchCompilerGHCParserLexer ghcFlavor = do
  when (ghcSeries ghcFlavor >= GHC_9_12) $ do
    writeFile "compiler/GHC/Parser/Lexer.x"
      . replace "{-# INLINE alexScanUser #-}" ""
      =<< readFile' "compiler/GHC/Parser/Lexer.x"

applyPatchCompilerGHCUnitTypes :: GhcFlavor -> IO ()
applyPatchCompilerGHCUnitTypes ghcFlavor = do
  when (ghcFlavor == Ghc9101) $ do
    writeFile "compiler/GHC/Unit/Types.hs"
      . replace
        "import Control.DeepSeq"
        "import Control.DeepSeq (NFData(..))"
      =<< readFile' "compiler/GHC/Unit/Types.hs"

-- Data type representing an approximately parsed Cabal file.
data Cabal = Cabal
  { cabalDir :: FilePath, -- the directory this file exists in
    cabalFields :: [(String, [String])] -- the key/value pairs it contains
  }

-- Given a file, produce the key/value pairs it contains (approximate
-- but good enough).
readCabalFile :: FilePath -> IO Cabal
readCabalFile file = do
  src <- readFile' file
  let fields = repeatedly f $ wordsBy (\x -> isSpace x || x == ',') $ unlines $ filter (not . isIf) $ map trimComment $ lines src
  return $ Cabal (takeDirectory file) fields
  where
    isIf x = "if " `isPrefixOf` trim x
    trimComment x = maybe x fst $ stripInfix "--" x
    f (x : xs) = let (a, b) = break (":" `isSuffixOf`) xs in ((lower x, a), b)
    f [] = error "readCabalFile: unexpected empty file"

-- Ask a Cabal file for a field.
askCabalField :: Cabal -> String -> [String]
askCabalField cbl x = concatMap snd $ filter ((==) x . fst) $ cabalFields cbl

-- Ask a Cabal file for files, relative to the underlying Cabal file.
askCabalFiles :: Cabal -> String -> [String]
askCabalFiles cbl x = map (cabalDir cbl </>) $ askCabalField cbl x

-- Harvest a field from a set of Cabal files (such as
-- 'exposed-modules').
askField :: [Cabal] -> String -> [String]
askField from x = nubSort $ concatMap (`askCabalField` x) from

-- Harvest a set of files from a set of Cabal files (such as the
-- 'hs-sourc-dirs').
askFiles :: [Cabal] -> String -> [String]
askFiles from x = nubSort $ concatMap (`askCabalFiles` x) from

-- Some often used string manipulation utilities.
indent :: [String] -> [String]
indent = map ("    " ++)

indent2 :: [String] -> [String]
indent2 = indent . indent

indent3 :: [String] -> [String]
indent3 = indent . indent . indent

withCommas :: Data.List.NonEmpty.NonEmpty String -> Data.List.NonEmpty.NonEmpty String
withCommas ms = Data.List.NonEmpty.fromList $ reverse (Data.List.NonEmpty.head ms' : map (++ ",") (Data.List.NonEmpty.tail ms'))
  where
    ms' :: Data.List.NonEmpty.NonEmpty String
    ms' = Data.List.NonEmpty.reverse ms

-- For each version of GHC, there is a minimum version build compiler
-- to bootstrap it. For example, for ghc-9.0.1, you need minimum ghc
-- version ghc-8.8.1 to build it. We want to try to arrange to make a
-- Cabal build plan impossible for ghc-lib flavor/ build compiler
-- version combinations that don't make sense. The idea here is to use
-- the base library version as a proxy for the minimum compiler
-- version.
baseBounds :: GhcFlavor -> String
baseBounds = \case
  -- ghc >= 8.4.4
  DaGhc881 -> "base >= 4.11 && < 4.16" -- unlike upstream GHC 8.8, the DA fork does work with ghc-8.10.1 and ghc-9.0.2
  Ghc881 -> "base >= 4.11 && < 4.14" -- [ghc-8.4.4, ghc-8.10.1)
  Ghc882 -> "base >= 4.11 && < 4.14"
  Ghc883 -> "base >= 4.11 && < 4.14"
  Ghc884 -> "base >= 4.11 && < 4.14"
  Ghc8101 -> "base >= 4.12 && < 4.15" -- [ghc-8.6.5, ghc-9.0.1)
  Ghc8102 -> "base >= 4.12 && < 4.15"
  Ghc8103 -> "base >= 4.12 && < 4.15"
  Ghc8104 -> "base >= 4.12 && < 4.15"
  Ghc8105 -> "base >= 4.12 && < 4.15"
  Ghc8106 -> "base >= 4.12 && < 4.15"
  Ghc8107 -> "base >= 4.12 && < 4.15"
  Ghc901 -> "base >= 4.13 && < 4.16" -- [ghc-8.8.1, ghc-9.2.1)
  Ghc902 -> "base >= 4.13 && < 4.16" -- [ghc-8.8.1, ghc-9.2.1)

  -- ghc-9.2.1, base-4.16.0.0
  -- ghc-9.2.2, base-4.16.1.0
  -- ghc-9.2.2, base-4.16.2.0
  -- ghc-9.2.4, base-4.16.3.0
  -- ghc-9.2.5, ghc-9.2.6, ghc-9.2.7, ghc-9.2.8 ship with base-4.16.4.0
  Ghc921 -> "base >= 4.14 && < 4.16.1" -- [ghc-8.10.1, ghc-9.2.2)
  Ghc922 -> "base >= 4.14 && < 4.16.2" -- [ghc-8.10.1, ghc-9.2.3)
  Ghc923 -> "base >= 4.14 && < 4.16.3" -- [ghc-8.10.1, ghc-9.2.4)
  Ghc924 -> "base >= 4.14 && < 4.16.4" -- [ghc-8.10.1, ghc-9.2.5)
  Ghc925 -> "base >= 4.14 && < 4.17" -- [ghc-8.10.1, ghc-9.4.1)
  Ghc926 -> "base >= 4.14 && < 4.17" -- [ghc-8.10.1, ghc-9.4.1)
  Ghc927 -> "base >= 4.14 && < 4.17" -- [ghc-8.10.1, ghc-9.4.1)
  Ghc928 -> "base >= 4.14 && < 4.17" -- [ghc-8.10.1, ghc-9.4.1)
  -- ghc-9.4.1, ghc-9.4.2, ghc-9.4.3, ghc-9.4.4 all ship with
  -- base-4.17.0.0, ghc-9.4.5 has base-4.17.1.0, ghc-9.4.6 has
  -- base-4.17.2.0
  Ghc941 -> "base >= 4.15 && < 4.18" -- [ghc-9.0.1, ghc-9.6.1)
  Ghc942 -> "base >= 4.15 && < 4.18" -- [ghc-9.0.1, ghc-9.6.1)
  Ghc943 -> "base >= 4.15 && < 4.18" -- [ghc-9.0.1, ghc-9.6.1)
  Ghc944 -> "base >= 4.15 && < 4.18" -- [ghc-9.0.1, ghc-9.6.1)
  Ghc945 -> "base >= 4.15 && < 4.18" -- [ghc-9.0.1, ghc-9.6.1)
  Ghc946 -> "base >= 4.15 && < 4.18" -- [ghc-9.0.1, ghc-9.6.1)
  Ghc947 -> "base >= 4.15 && < 4.18" -- [ghc-9.0.1, ghc-9.6.1)
  Ghc948 -> "base >= 4.15 && < 4.18" -- [ghc-9.0.1, ghc-9.6.1)

  -- require bytestring >= 0.11.3 which rules out ghc-9.2.1
  -- base-4.18.0
  Ghc961 -> "base >= 4.16.1 && < 4.19" -- [ghc-9.2.2, ghc-9.8.1)
  Ghc962 -> "base >= 4.16.1 && < 4.19" -- [ghc-9.2.2, ghc-9.8.1)
  Ghc963 -> "base >= 4.16.1 && < 4.19" -- [ghc-9.2.2, ghc-9.8.1)
  Ghc964 -> "base >= 4.16.1 && < 4.19" -- [ghc-9.2.2, ghc-9.8.1)
  Ghc965 -> "base >= 4.16.1 && < 4.19" -- [ghc-9.2.2, ghc-9.8.1)
  Ghc966 -> "base >= 4.16.1 && < 4.19" -- [ghc-9.2.2, ghc-9.8.1)

  -- base-4.19.0.0, ghc-prim-0.11.0
  Ghc981 -> "base >= 4.17 && < 4.19.1" -- [ghc-9.4.1, ghc-9.8.2)
  -- base-4.19.1.0
  Ghc982 -> "base >= 4.17 && < 4.19.2" -- [ghc-9.4.1, ghc-9.10.1)
  -- base-4.19.2.0
  Ghc983 -> "base >= 4.17 && < 4.20" -- [ghc-9.4.1, ghc-9.10.1)
  Ghc984 -> "base >= 4.17 && < 4.20" -- [ghc-9.4.1, ghc-9.10.1)
  -- base-4.20.0.0
  Ghc9101 -> "base >= 4.18 && < 4.21" -- [ghc-9.6.1, ghc-9.12.1)
  -- base-4.21.0.0
  Ghc9121 -> "base >= 4.19 && < 4.22" -- [ghc-9.8.1, ghc-9.14.1)
  GhcMaster ->
    -- e.g. "9.11.20230119"
    -- (c.f. 'rts/include/ghcversion.h')
    "base >= 4.19 && < 4.22" -- [ghc-9.8.1, ghc-9.14.1)

-- Common build dependencies.
commonBuildDepends :: GhcFlavor -> Data.List.NonEmpty.NonEmpty String
commonBuildDepends ghcFlavor =
  Data.List.NonEmpty.fromList $ base ++ specific ++ conditional ++ shared
  where
    -- base
    base = [baseBounds ghcFlavor]
    specific
      | ghcSeries ghcFlavor >= GHC_9_12 =
          [ "ghc-prim > 0.2 && < 0.14",
            "containers >= 0.6.2.1 && < 0.8",
            "bytestring >= 0.11.4 && < 0.13",
            "time >= 1.4 && < 1.15",
            "filepath >= 1.5 && < 1.6",
            "os-string >= 2.0.1 && < 2.1"
          ]
      | ghcSeries ghcFlavor >= GHC_9_10 =
          [ "ghc-prim > 0.2 && < 0.12",
            "containers >= 0.6.2.1 && < 0.8",
            "bytestring >= 0.11.4 && < 0.13",
            "time >= 1.4 && < 1.15",
            "filepath >= 1 && < 1.6"
          ]
      | ghcSeries ghcFlavor >= GHC_9_8 =
          [ "ghc-prim > 0.2 && < 0.12",
            "containers >= 0.6.2.1 && < 0.8",
            "bytestring >= 0.11.4 && < 0.13",
            "time >= 1.4 && < 1.15",
            "filepath >= 1 && < 1.6"
          ]
      | ghcSeries ghcFlavor >= GHC_9_6 =
          [ "ghc-prim > 0.2 && < 0.11",
            "containers >= 0.6.2.1 && < 0.7",
            "bytestring >= 0.11.3 && < 0.12",
            "time >= 1.4 && < 1.13",
            "filepath >= 1 && < 1.5"
          ]
      | ghcSeries ghcFlavor >= GHC_9_4 =
          [ "ghc-prim > 0.2 && < 0.10",
            "containers >= 0.5 && < 0.7",
            "bytestring >= 0.10 && < 0.12",
            "time >= 1.4 && < 1.13",
            "filepath >= 1 && < 1.5"
          ]
      | ghcSeries ghcFlavor >= GHC_9_2 =
          [ "ghc-prim > 0.2 && < 0.9",
            "containers >= 0.5 && < 0.7",
            "bytestring >= 0.9 && < 0.12",
            "time >= 1.4 && < 1.12",
            "filepath >= 1 && < 1.5"
          ]
      | otherwise =
          [ "ghc-prim > 0.2 && < 0.8",
            "containers >= 0.5 && < 0.7",
            "bytestring >= 0.9 && < 0.11",
            "time >= 1.4 && < 1.10",
            "filepath >= 1 && < 1.5"
          ]
    conditional
      | ghcSeries ghcFlavor >= GHC_9_0 =
          [ "exceptions == 0.10.*",
            "parsec"
          ]
      | otherwise =
          []
    -- shared for all flavors
    shared =
      [ "binary == 0.8.*",
        "directory >= 1 && < 1.4",
        "array >= 0.1 && < 0.6",
        "deepseq >= 1.4 && < 1.6",
        "pretty == 1.1.*",
        "transformers >= 0.5 && < 0.7",
        "process >= 1 && < 1.7"
      ]

ghcLibParserBuildDepends :: GhcFlavor -> Data.List.NonEmpty.NonEmpty String
ghcLibParserBuildDepends = commonBuildDepends

ghcLibBuildDepends :: GhcFlavor -> Data.List.NonEmpty.NonEmpty String
ghcLibBuildDepends ghcFlavor =
  commonBuildDepends ghcFlavor
    <> Data.List.NonEmpty.fromList
      ( join
          [ ["stm" | ghcSeries ghcFlavor >= GHC_9_4],
            ["semaphore-compat" | ghcSeries ghcFlavor >= GHC_9_8],
            [ "rts",
              "hpc >= 0.6 && < 0.8",
              "ghc-lib-parser" -- we rely on this being last (in CI.hs:
              -- 'patchConstraints')!
            ]
          ]
      )

libBinParserLibModules :: GhcFlavor -> IO ([Cabal], [Cabal], [String], [String])
libBinParserLibModules ghcFlavor = do
  lib <- mapM readCabalFile (cabalFileLibraries ghcFlavor)
  bin <- readCabalFile cabalFileBinary
  parserModules <- filterGhcInternalModules <$> calcParserModules ghcFlavor
  libModules <- filterGhcInternalModules <$> calcLibModules ghcFlavor
  return (lib, [bin], parserModules, libModules)
  where
    filterGhcInternalModules :: [String] -> [String]
    filterGhcInternalModules = filter (not . isInfixOf "GHC.Internal")

happyBounds :: GhcFlavor -> String
happyBounds ghcFlavor
  | series < GHC_9_8 = "< 1.21"
  | otherwise = "== 1.20.* || == 2.0.2 || >= 2.1.2 && < 2.2" -- c.f. m4/fptools_happy.m4
  where
    series = ghcSeries ghcFlavor

-- Produces a ghc-lib Cabal file.
generateGhcLibCabal :: GhcFlavor -> [String] -> IO ()
generateGhcLibCabal ghcFlavor customCppOpts = do
  (lib, _bin, parserModules, libModules) <- libBinParserLibModules ghcFlavor
  let nonParserModules =
        Set.toList
          ( Set.difference
              (Set.fromList libModules)
              (Set.fromList parserModules)
          )
  {- Alternative:
     ```
       let nonParserModules =
            Set.toList (Set.difference
            (Set.fromList $ askField lib "exposed-modules:" )
            (Set.fromList parserModules))
      ```
  -}
  let hsSrcDirs = replace ["libraries/ghc-boot-th/../ghc-internal/src"] [] (ghcLibHsSrcDirs False ghcFlavor lib)
  let includeDirs = replace ["libraries/ghc-internal/include"] [] (ghcLibIncludeDirs ghcFlavor)
  writeFile "ghc-lib.cabal" . unlines . map trimEnd . join $
    [ [ "cabal-version: 3.0",
        "build-type: Simple",
        "name: ghc-lib",
        "version: 0.1.0",
        "license: BSD-3-Clause",
        "license-file: LICENSE",
        "category: Development",
        "author: The GHC Team and Digital Asset",
        "maintainer: Digital Asset",
        "synopsis: The GHC API, decoupled from GHC versions",
        "description: A package equivalent to the @ghc@ package, but which can be loaded on many compiler versions.",
        "homepage: https://github.com/digital-asset/ghc-lib",
        "bug-reports: https://github.com/digital-asset/ghc-lib/issues",
        "data-dir: " ++ dataDir,
        "data-files:"
      ],
      indent (dataFiles ghcFlavor),
      ["extra-source-files:"],
      indent (performExtraFilesSubstitutions ghcFlavor ghcLibExtraFiles),
      [ "source-repository head",
        "    type: git",
        "    location: git@github.com:digital-asset/ghc-lib.git"
      ],
      [ "flag threaded-rts",
        "    default: True",
        "    manual: True",
        "    description: Pass -DTHREADED_RTS to the C toolchain"
      ],
      ["library"],
      ["    default-language: Haskell2010" | ghcFlavor < Ghc9101],
      ["    default-language: GHC2021" | ghcFlavor >= Ghc9101],
      [ "    exposed: False",
        "    include-dirs:"
      ],
      indent2 includeDirs,
      [ "    if impl(ghc >= 8.8.1)",
        "        ghc-options: -fno-safe-haskell"
      ],
      [ "    if flag(threaded-rts)",
        "        ghc-options: -fobject-code -package=ghc-boot-th -optc-DTHREADED_RTS",
        "        cc-options: -DTHREADED_RTS",
        "        cpp-options: -DTHREADED_RTS " ++ generateCppOpts ghcFlavor customCppOpts,
        "    else",
        "        ghc-options: -fobject-code -package=ghc-boot-th",
        "        cpp-options: " ++ generateCppOpts ghcFlavor customCppOpts
      ],
      [ "    if !os(windows)",
        "        build-depends: unix",
        "    else",
        "        build-depends: Win32",
        "    build-depends:"
      ],
      indent2 (Data.List.NonEmpty.toList (withCommas (ghcLibBuildDepends ghcFlavor))),
      ["    build-tool-depends: alex:alex >= 3.1, " ++ "happy:happy " ++ happyBounds ghcFlavor],
      ["    other-extensions:"],
      indent2 (askField lib "other-extensions:"),
      ["    default-extensions:"],
      indent2 (askField lib "default-extensions:"),
      ["    hs-source-dirs:"],
      indent2 hsSrcDirs,
      [ "    autogen-modules:",
        "        Paths_ghc_lib"
      ],
      ["    reexported-modules:"],
      indent2 (Data.List.NonEmpty.toList (withCommas (Data.List.NonEmpty.fromList $ nubSort parserModules))),
      if ghcSeries ghcFlavor > GHC_9_10 then
        join [
               ["    if impl(ghc < 9.12.1)"],
               ["      reexported-modules:"],
               indent3 (Data.List.NonEmpty.toList (withCommas (Data.List.NonEmpty.fromList [  "GHC.Internal.ForeignSrcLang", "GHC.Internal.LanguageExtensions", "GHC.Internal.Lexeme", "GHC.Internal.TH.Syntax", "GHC.Internal.TH.Ppr", "GHC.Internal.TH.PprLib", "GHC.Internal.TH.Lib.Map"])))
             ]
      else [],
      [ "    exposed-modules:",
        "        Paths_ghc_lib"
      ],
      indent2 (nubSort nonParserModules)
    ]
  putStrLn "# Generating 'ghc-lib.cabal'... Done!"

generateCppOpts :: GhcFlavor -> [String] -> String
generateCppOpts ghcFlavor customCppOpts =
  unwords $
    [ ghcStageDef ghcFlavor,
      ghcInGhciDef ghcFlavor,
      bootstrapTh ghcFlavor
    ]
      ++ customCppOpts
  where
    ghcInGhciDef, ghcStageDef :: GhcFlavor -> String
    ghcInGhciDef = \case f | ghcSeries f >= GHC_9_2 -> ""; _ -> "-DGHC_IN_GHCI"
    ghcStageDef = \case f | ghcSeries f >= GHC_8_10 -> ""; _ -> "-DSTAGE=2"
    bootstrapTh = \case f | ghcSeries f <= GHC_9_10 -> ""; _ -> "-DBOOTSTRAP_TH"

-- Perform a set of specific substitutions on the given list of files.
performExtraFilesSubstitutions :: GhcFlavor -> (GhcFlavor -> [FilePath]) -> [FilePath]
performExtraFilesSubstitutions ghcFlavor files =
  foldl' sub (files ghcFlavor) . join $
    [ [("rts/include/ghcversion.h", Nothing) | ghcSeries ghcFlavor >= GHC_9_4],
      [(hadrianGeneratedRoot ghcFlavor </> "ghcversion.h", Nothing) | ghcSeries ghcFlavor < GHC_9_4],
      [((stage0Rts </> "include") </> "DerivedConstants.h", Just $ (stage0Rts </> "include") </> "GhclibDerivedConstants.h") | ghcSeries ghcFlavor >= GHC_9_4],
      [(hadrianGeneratedRoot ghcFlavor </> "DerivedConstants.h", Just $ hadrianGeneratedRoot ghcFlavor </> "GhclibDerivedConstants.h") | ghcSeries ghcFlavor < GHC_9_4],
      [("compiler" </> "HsVersions.h", Just $ "compiler" </> "GhclibHsVersions.h") | ghcSeries ghcFlavor < GHC_9_4]
    ]
  where
    sub :: (Eq a) => [a] -> (a, Maybe a) -> [a]
    sub xs (s, r) = replace [s] (maybeToList r) xs

-- Produces a ghc-lib-parser Cabal file.
generateGhcLibParserCabal :: GhcFlavor -> [String] -> IO ()
generateGhcLibParserCabal ghcFlavor customCppOpts = do
  (lib, _bin, parserModules, _) <- libBinParserLibModules ghcFlavor
  -- Remove 'ghc-interal/src', 'ghc-boot-th/internal' etc. They are
  -- dealt with later explicitly.
  let hsSrcDirs = filter (not . isInfixOf "internal") $ ghcLibParserHsSrcDirs False ghcFlavor lib
  let includeDirs = filter (/= "libraries/ghc-internal/include") $ ghcLibParserIncludeDirs ghcFlavor
  keepCAFsForCHCiExists <- doesFileExist "compiler/cbits/keepCAFsForGHCi.c"
  writeFile "ghc-lib-parser.cabal" . unlines . map trimEnd . join $
    [ [ "cabal-version: 3.0",
        "build-type: Simple",
        "name: ghc-lib-parser",
        "version: 0.1.0",
        "license: BSD-3-Clause",
        "license-file: LICENSE",
        "category: Development",
        "author: The GHC Team and Digital Asset",
        "maintainer: Digital Asset",
        "synopsis: The GHC API, decoupled from GHC versions",
        "description: A package equivalent to the @ghc@ package, but which can be loaded on many compiler versions.",
        "homepage: https://github.com/digital-asset/ghc-lib",
        "bug-reports: https://github.com/digital-asset/ghc-lib/issues",
        "data-dir: " ++ dataDir,
        "data-files:"
      ],
      indent (dataFiles ghcFlavor),
      ["extra-source-files:"],
      indent (performExtraFilesSubstitutions ghcFlavor ghcLibParserExtraFiles),
      [ "source-repository head",
        "    type: git",
        "    location: git@github.com:digital-asset/ghc-lib.git"
      ],
      [ "flag threaded-rts",
        "  default: True",
        "  manual: True",
        "  description: Pass -DTHREADED_RTS to the C toolchain"
      ],
      ["library"],
      ["    default-language: Haskell2010" | ghcFlavor < Ghc9101],
      ["    default-language: GHC2021" | ghcFlavor >= Ghc9101],
      [ "    exposed: False",
        "    include-dirs:"
      ],
      indent2 includeDirs,
      [ "    if impl(ghc >= 8.8.1)",
        "        ghc-options: -fno-safe-haskell"
      ],
      [ "    if flag(threaded-rts)",
        "        ghc-options: -fobject-code -package=ghc-boot-th -optc-DTHREADED_RTS",
        "        cc-options: -DTHREADED_RTS",
        "        cpp-options: -DTHREADED_RTS " ++ generateCppOpts ghcFlavor customCppOpts,
        "    else",
        "        ghc-options: -fobject-code -package=ghc-boot-th",
        "        cpp-options: " ++ generateCppOpts ghcFlavor customCppOpts
      ],
      [ "    if !os(windows)",
        "        build-depends: unix",
        "    else",
        "        build-depends: Win32",
        "    build-depends:"
      ],
      indent2 (Data.List.NonEmpty.toList (withCommas (ghcLibParserBuildDepends ghcFlavor))),
      [ "    if impl(ghc >= 9.10)",
        "      build-depends: ghc-internal"
      ],
      ["    build-tool-depends: alex:alex >= 3.1, " ++ "happy:happy " ++ happyBounds ghcFlavor],
      ["    other-extensions:"],
      indent2 (askField lib "other-extensions:"),
      ["    default-extensions:"],
      indent2 (askField lib "default-extensions:"),
      ["    if impl(ghc >= 9.2.2) "], -- cabal >= 3.6.0
      ["      cmm-sources:"],
      indent3 ["libraries/ghc-heap/cbits/HeapPrim.cmm"],
      ["    else"],
      ["      c-sources:"],
      indent3 ["libraries/ghc-heap/cbits/HeapPrim.cmm"],
      ["    c-sources:"],
      indent2 ["compiler/cbits/genSym.c"],
      indent2 ["compiler/cbits/cutils.c" | ghcSeries ghcFlavor >= GHC_9_0],
      indent2 ["compiler/parser/cutils.c" | ghcSeries ghcFlavor < GHC_9_0],
      indent2 ["compiler/cbits/keepCAFsForGHCi.c" | keepCAFsForCHCiExists],
      ["    hs-source-dirs:"],
      indent2 hsSrcDirs,
      ["    autogen-modules:"],
      indent2 [x | ghcSeries ghcFlavor >= GHC_9_0, x <- ["GHC.Parser.Lexer", "GHC.Parser"]],
      indent2 [x | ghcSeries ghcFlavor < GHC_9_0, x <- ["Lexer", "Parser"]],
      ["    exposed-modules:"],
      indent2 parserModules,
      if ghcSeries ghcFlavor > GHC_9_10 then
        join [
               ["    if impl(ghc < 9.12.1)"],
               ["      hs-source-dirs:"],
               ["        libraries/ghc-internal/src"],
               ["        libraries/ghc-boot-th-internal"],
               ["      exposed-modules:"],
               indent3 [  "GHC.Internal.ForeignSrcLang", "GHC.Internal.LanguageExtensions", "GHC.Internal.Lexeme", "GHC.Internal.TH.Syntax", "GHC.Internal.TH.Ppr", "GHC.Internal.TH.PprLib", "GHC.Internal.TH.Lib.Map"]
             ]
      else []
    ]
  putStrLn "# Generating 'ghc-lib-parser.cabal'... Done!"

-- Run Hadrian to build the things that the Cabal files need.
generatePrerequisites :: GhcFlavor -> IO ()
generatePrerequisites ghcFlavor = do
  when
    (ghcSeries ghcFlavor < GHC_8_10)
    ( -- Workaround a Windows bug present in at least 8.4.3. See
      -- http://haskell.1045720.n5.nabble.com/msys-woes-td5898334.html
      writeFile "./mk/get-win32-tarballs.sh"
        . replace
          "$curl_cmd || echo \"Checking repo.msys2.org instead of Haskell.org...\" && $curl_cmd_bnk || {"
          "$curl_cmd || (echo \"Checking repo.msys2.org instead of Haskell.org...\" && $curl_cmd_bnk) || {"
        =<< readFile' "./mk/get-win32-tarballs.sh"
    )

  system_ "bash -c ./boot"
  system_ "bash -c \"./configure --enable-tarballs-autodownload\""
  withCurrentDirectory "hadrian" $ do
    system_ "cabal build exe:hadrian --ghc-options=-j"
    system_ . unwords . join $
      [ [ "cabal run exe:hadrian --",
          "--directory=..",
          "--build-root=ghc-lib"
        ],
        ["--bignum=native" | ghcSeries ghcFlavor >= GHC_9_0],
        ["--integer-simple" | ghcSeries ghcFlavor < GHC_9_0],
        ghcLibParserExtraFiles ghcFlavor,
        map (dataDir </>) $ dataFiles ghcFlavor
      ]

-- Given an Hsc, Alex, or Happy file, generate a placeholder module
-- with the same module imports.
genPlaceholderModule :: FilePath -> IO ()
genPlaceholderModule m = do
  (name, imports) <- withFile m ReadMode $ \h -> do
    name <- parseModuleName h
    imports <- parseModuleImports h []
    pure (name, imports)
  let fname = placeholderModulesDir </> T.unpack (T.replace "." "/" name <> ".hs")
  createDirectoryIfMissing True (takeDirectory fname)
  withFile fname WriteMode $ \h -> do
    T.hPutStrLn h $ "module " <> name <> " where"
    let extra = extraImports $ takeExtension fname
    forM_ (imports ++ extra) $ \i -> T.hPutStrLn h $ "import " <> i
  pure ()
  where
    parseModuleName :: Handle -> IO T.Text
    parseModuleName h = do
      l <- T.hGetLine h
      if "module " `T.isPrefixOf` l
        then case T.words l of
          _module : name : _ -> pure $ T.takeWhile (/= '(') name
          _ -> fail $ "Cannot parse module name of " ++ m
        else
          parseModuleName h

    parseModuleImports :: Handle -> [T.Text] -> IO [T.Text]
    parseModuleImports h acc = handleEof acc $ do
      l <- T.hGetLine h
      acc <-
        if "import " `T.isPrefixOf` l
          then case T.words l of
            _import : "::" : _ -> pure acc -- Skip `import :: { ... }` in Parser.y
            _import : "qualified" : name : _ -> do
              pure $ T.takeWhile (/= '(') name : acc
            _import : name : _ -> do
              pure $ T.takeWhile (/= '(') name : acc
            _ -> fail $ "Cannot parse import in " ++ m ++ " in line " ++ T.unpack l
          else
            pure acc
      parseModuleImports h acc

    handleEof :: a -> IO a -> IO a
    handleEof acc = handle $ \e ->
      if isEOFError e then pure acc else ioError e

    extraImports :: String -> [T.Text]
    extraImports ".x" = ["Data.Array", "Data.Array.Base", "GHC.Exts"] -- Alex adds these imports
    extraImports ".y" = ["Data.Array", "GHC.Exts"] -- Happy adds these imports
    extraImports _ = []

genPlaceholderModules :: FilePath -> IO ()
genPlaceholderModules = loop
  where
    loop fp = do
      isDir <- doesDirectoryExist fp
      if isDir
        then do
          contents <- listDirectory fp
          mapM_ (loop . (fp </>)) contents
        else when (takeExtension fp `elem` [".x", ".y", ".hsc"]) $ genPlaceholderModule fp
