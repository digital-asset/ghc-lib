-- Copyright (c) 2019-2023, Digital Asset (Switzerland) GmbH and/or
-- its affiliates. All rights reserved.  SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
#if __GLASGOW_HASKELL__ >= 902
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
#endif

module Ghclibgen (
    applyPatchHeapClosures
  , applyPatchAclocal
  , applyPatchHsVersions
  , applyPatchGhcPrim
  , applyPatchHaddockHs
  , applyPatchRtsBytecodes
  , applyPatchGHCiInfoTable
  , applyPatchGHCiMessage
  , applyPatchDerivedConstants
  , applyPatchDisableCompileTimeOptimizations
  , applyPatchRtsIncludePaths
  , applyPatchStage
  , applyPatchNoMonoLocalBinds
  , applyPatchCmmParseNoImplicitPrelude
  , applyPatchHadrianStackYaml
  , applyPatchTemplateHaskellCabal
  , applyPatchSystemSemaphore
  , generatePrerequisites
  , mangleCSymbols
  , generateGhcLibCabal
  , generateGhcLibParserCabal
) where

import Control.Exception (handle)
import Control.Monad
import System.Process.Extra
import System.FilePath hiding ((</>), normalise, dropTrailingPathSeparator)
import System.FilePath.Posix((</>), normalise, dropTrailingPathSeparator) -- Make sure we generate / on all platforms.
import System.Directory
import System.Directory.Extra
import System.IO.Error (isEOFError)
import System.IO.Extra
import Data.List.Extra hiding (find)
import Data.Char
import Data.Maybe
import Data.Ord
import qualified Data.Set as Set

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson.Types(parse, Result(..))
#if MIN_VERSION_aeson(2, 0, 2)
import Data.Aeson.KeyMap(toHashMap)
#endif
import qualified Data.Yaml as Y
import Data.Yaml (ToJSON(..), (.:?), (.!=))
import qualified Data.HashMap.Strict as HMS

import GhclibgenFlavor

-- Constants.

-- | Cabal files from libraries inside GHC that are merged together.
cabalFileLibraries :: [FilePath]
cabalFileLibraries = [
      "libraries/template-haskell/template-haskell.cabal"
    , "libraries/ghc-heap/ghc-heap.cabal"
    , "libraries/ghc-boot-th/ghc-boot-th.cabal"
    , "libraries/ghc-boot/ghc-boot.cabal"
    , "libraries/ghci/ghci.cabal"
    , "compiler/ghc.cabal"
    ]

-- | C-preprocessor "include dirs" for 'ghc-lib-parser'.
ghcLibParserIncludeDirs :: GhcFlavor -> [FilePath]
ghcLibParserIncludeDirs ghcFlavor = (case ghcSeries ghcFlavor of
  series | series >= Ghc94 -> [ "rts/include" ] -- ghcconfig.h, ghcversion.h
  series | series < Ghc94 -> [ "includes" ] -- ghcconfig.h, MachDeps.h, MachRegs.h, CodeGen.Platform.hs
  _ -> error "ghcLibParserIncludeDirs: impossible case!"
  ) ++
  [ hadrianGeneratedRoot ghcFlavor, stage0Compiler, "compiler" ] ++
  [ "compiler/utils" | ghcSeries ghcFlavor < Ghc810 ]

-- Sort by length so the longest paths are at the front. We do this
-- so that in 'calcParserModules', longer substituions are performed
-- before shorter ones (and bad things will happen if that were
-- not the case).
sortDiffListByLength :: Set.Set FilePath -> Set.Set FilePath -> [FilePath]
sortDiffListByLength all excludes =
  nubOrd . sortOn (Down . length) $ Set.toList (Set.difference all excludes)

-- | The "hs-source-dirs" for 'ghc-lib-parser' (actually used in two
-- contexts, 'ghc-lib-parser.cabal' generation and when calculating
-- the set of parser modules).
ghcLibParserHsSrcDirs :: Bool -> GhcFlavor -> [Cabal] -> [FilePath]
ghcLibParserHsSrcDirs forParserDepends ghcFlavor lib =
  let all = Set.fromList $
        [ stage0Compiler ] ++
        -- The subtlety here is that, when calculating parser modules
        -- (i.e. 'forParserDepends == True') via 'ghc -M' we need
        -- these directories poplulated with generated intermediate
        -- files ('ghc -M' isn't smart enough to "read through" .hsc
        -- files). When we sdist though, we exclude the generated
        -- files (rather, we ship their .hsc sources and the generated
        -- files are physically deleted at that point) and
        -- consequently, these directories are empty causing Cabal to
        -- warn (which is then going to cause problems with uploading
        -- to Hackage for example). Thus, when writing
        -- 'ghc-lib-parser.cabal' (i.e. 'forParserDepends == False'),
        -- we exclude these directories.
        [ dir | forParserDepends, dir <- [ stage0Ghci, stage0GhcHeap ] ] ++
        [ stage0GhcBoot | ghcSeries ghcFlavor >= Ghc810 ] ++
        map takeDirectory cabalFileLibraries ++
        map (dropTrailingPathSeparator . normalise) (askFiles lib "hs-source-dirs:")

      excludes = Set.fromList $
        ("compiler" </>) <$> (
          [ "codeGen", "hieFile", "llvmGen" , "rename", "stgSyn", "stranal" ] ++
          [ d | ghcSeries ghcFlavor >= Ghc90, d <- [ "typecheck", "specialise", "cmm" ] ] ++
          [ "nativeGen" | ghcSeries ghcFlavor < Ghc90 ] ++
          [ "deSugar" | ghcSeries ghcFlavor >= Ghc810 && not forParserDepends ]
        )
  in sortDiffListByLength all excludes -- Very important. See the comment on 'sortDiffListByLength' above.

-- | C-preprocessor "include dirs" for 'ghc-lib'.
ghcLibIncludeDirs :: GhcFlavor -> [FilePath]
ghcLibIncludeDirs = ghcLibParserIncludeDirs -- Needs checking (good enough for now).

-- | The "hs-source-dirs" for 'ghc-lib'.
ghcLibHsSrcDirs :: GhcFlavor -> [Cabal] -> [FilePath]
ghcLibHsSrcDirs ghcFlavor lib =
  let all = Set.fromList $
        [ stage0Compiler ] ++
        [ stage0GhcBoot | ghcSeries ghcFlavor >= Ghc810 ] ++
        map takeDirectory cabalFileLibraries ++
        map (dropTrailingPathSeparator . normalise) (askFiles lib "hs-source-dirs:")
      excludes = Set.fromList $
        (("compiler" </>) <$> [ "basicTypes", "parser", "types" ]) ++ (("libraries" </>) <$> [ "ghc-boot-th", "ghc-heap" ]) ++ [ "compiler/cmm" | ghcFlavor >= Ghc901 ]
  in sortDiffListByLength all excludes -- Not so important. Here for symmetry with 'ghcLibParserHsSrcDirs' I think.

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

-- | Sources generated by Hadrian are written under this directory.
hadrianGeneratedRoot :: GhcFlavor -> FilePath
hadrianGeneratedRoot = \case
  f | f >= Ghc8101 -> stage0Lib
  _ -> ghcLibGeneratedPath

-- |'dataFiles' is a list of files to be installed for run-time use by
-- the package.
dataFiles :: GhcFlavor -> [FilePath]
dataFiles ghcFlavor =
  -- From ghc/ghc.mk: "The GHC programs need to depend on all the
  -- helper programs they might call and the settings files they
  -- use."
  [ "settings", "llvm-targets", "llvm-passes"] ++
  [ "platformConstants" | ghcSeries ghcFlavor < Ghc92 ]

-- | See 'hadrian/src/Rules/Generate.hs'.

cabalFileDependencies :: GhcFlavor -> [FilePath]
cabalFileDependencies ghcFlavor =
  [ f | ghcSeries ghcFlavor > Ghc94, f <- cabalFileBinary : cabalFileLibraries ]

rtsDependencies :: GhcFlavor -> [FilePath]
rtsDependencies ghcFlavor =
  if ghcSeries ghcFlavor >= Ghc94 then
    ((stage0Rts </> "include") </>) <$> ["ghcautoconf.h", "ghcplatform.h", "DerivedConstants.h"]
  else
    includesDependencies ghcFlavor ++ derivedConstantsDependencies ghcFlavor
  where
    includesDependencies :: GhcFlavor -> [FilePath]
    includesDependencies ghcFlavor =
        (hadrianGeneratedRoot ghcFlavor </>) <$> [ "ghcautoconf.h", "ghcplatform.h", "ghcversion.h"]
    derivedConstantsDependencies :: GhcFlavor -> [FilePath]
    derivedConstantsDependencies ghcFlavor =
      (hadrianGeneratedRoot ghcFlavor </>) <$> ("DerivedConstants.h" : [ x | ghcSeries ghcFlavor <= Ghc90, x <- [ "GHCConstantsHaskellExports.hs", "GHCConstantsHaskellWrappers.hs", "GHCConstantsHaskellType.hs" ] ] )

compilerDependencies :: GhcFlavor -> [FilePath]
compilerDependencies ghcFlavor =
  (stage0Compiler </>) <$> [
      "primop-can-fail.hs-incl"
    , "primop-code-size.hs-incl"
    , "primop-commutable.hs-incl"
    , "primop-data-decl.hs-incl"
    , "primop-fixity.hs-incl"
    , "primop-has-side-effects.hs-incl"
    , "primop-list.hs-incl"
    , "primop-out-of-line.hs-incl"
    , "primop-primop-info.hs-incl"
    , "primop-strictness.hs-incl"
    , "primop-tag.hs-incl"
    , "primop-vector-tycons.hs-incl"
    , "primop-vector-tys-exports.hs-incl"
    , "primop-vector-tys.hs-incl"
    , "primop-vector-uniques.hs-incl"
    ] ++
    [ "primop-docs.hs-incl" | ghcSeries ghcFlavor >= Ghc90 ] ++
    [ "GHC/Platform/Constants.hs" | ghcSeries ghcFlavor >= Ghc92 ]

platformH :: GhcFlavor -> [FilePath]
platformH ghcFlavor =
  [ stage0Compiler </> "ghc_boot_platform.h" | ghcSeries ghcFlavor < Ghc810 ]

packageCode :: GhcFlavor -> [FilePath]
packageCode ghcFlavor =
  [ stage0Compiler </> "GHC/Settings/Config.hs" | ghcSeries ghcFlavor >= Ghc90 ] ++
  [ stage0Compiler </> "Config.hs" | ghcSeries ghcFlavor < Ghc90 ] ++
  [ stage0GhcBoot </> "GHC" </> x | ghcSeries ghcFlavor >= Ghc810,  x <- ["Version.hs", "Platform/Host.hs"] ]

fingerprint :: GhcFlavor -> [FilePath]
fingerprint ghcFlavor =
  [ "compiler/utils/Fingerprint.hsc" | ghcSeries ghcFlavor < Ghc810 ]

cHeaders :: GhcFlavor -> [String]
cHeaders ghcFlavor =
   [ f | ghcSeries ghcFlavor >= Ghc94, f <- (("rts/include" </>) <$> ["ghcconfig.h", "ghcversion.h" ]) ++ (("compiler" </>) <$>[ "MachRegs.h", "CodeGen.Platform.h", "Bytecodes.h", "ClosureTypes.h", "FunTypes.h", "Unique.h", "ghc-llvm-version.h" ]) ] ++
   [ f | ghcSeries ghcFlavor < Ghc94, f <- (("includes" </>)  <$> [ "MachDeps.h", "stg/MachRegs.h", "CodeGen.Platform.hs"]) ++ (("compiler" </>) <$> [ "Unique.h", "HsVersions.h" ]) ] ++
   [ f | ghcSeries ghcFlavor < Ghc810, f <- ("compiler" </>) <$> [ "nativeGen/NCG.h", "utils/md5.h"] ]

parsersAndLexers :: GhcFlavor -> [FilePath]
parsersAndLexers ghcFlavor = ("compiler" </>) <$>
   [ x |  ghcSeries ghcFlavor < Ghc90, x <- ("parser" </>) <$> [ "Parser.y", "Lexer.x"] ] ++
   [ x |  ghcSeries ghcFlavor >= Ghc90, x <- ("GHC" </>) <$> [ "Parser.y", "Parser/Lexer.x" ] ] ++
   [ x |  ghcSeries ghcFlavor >= Ghc94, x <- ("GHC" </>) <$> [ "Parser/HaddockLex.x", "Parser.hs-boot" ] ]

ghcLibParserExtraFiles :: GhcFlavor -> [FilePath]
ghcLibParserExtraFiles ghcFlavor =
  cabalFileDependencies ghcFlavor ++
  rtsDependencies ghcFlavor ++
  compilerDependencies ghcFlavor ++
  platformH ghcFlavor ++
  fingerprint ghcFlavor ++
  packageCode ghcFlavor ++
  parsersAndLexers ghcFlavor ++
  cHeaders ghcFlavor

ghcLibExtraFiles :: GhcFlavor -> [FilePath]
ghcLibExtraFiles ghcFlavor =
  rtsDependencies ghcFlavor ++
  compilerDependencies ghcFlavor ++
  platformH ghcFlavor ++
  fingerprint ghcFlavor ++
  cHeaders ghcFlavor

-- | We generate some "placeholder" modules in `calcParserModues` They
-- get written here.
placeholderModulesDir :: FilePath
placeholderModulesDir = "placeholder_modules"

-- | Calculate via `ghc -M` the list of modules that are required for
-- 'ghc-lib-parser'.
calcParserModules :: GhcFlavor -> IO [String]
calcParserModules ghcFlavor = do
  lib <- mapM readCabalFile cabalFileLibraries

  genPlaceholderModules "compiler"
  genPlaceholderModules "libraries/ghc-heap"
  genPlaceholderModules "libraries/ghci"
  when (ghcSeries ghcFlavor >= Ghc94) $ do
    copyFile "compiler/GHC/Parser.hs-boot" (placeholderModulesDir </> "GHC/Parser.hs-boot")

  let includeDirs = map ("-I" ++ ) (ghcLibParserIncludeDirs ghcFlavor)
      hsSrcDirs = ghcLibParserHsSrcDirs True ghcFlavor lib
      hsSrcIncludes = map ("-i" ++ ) (placeholderModulesDir : hsSrcDirs)
      -- See [Note: GHC now depends on exceptions package].
      cmd = unwords $
        [ "stack exec" ] ++
        [ "--package exceptions" | ghcSeries ghcFlavor == Ghc90 ] ++
        [ "--stack-yaml hadrian/stack.yaml" ] ++
        [ "-- ghc"
        , "-dep-suffix ''"
        , "-dep-makefile .parser-depends"
        , "-M"
        ] ++
        includeDirs ++
        [ "-ignore-package ghc"
        , "-ignore-package ghci"
        , "-package base"
        ] ++
        [ "-package exceptions" | ghcSeries ghcFlavor == Ghc90 ] ++
        hsSrcIncludes ++
        ((placeholderModulesDir </>) <$> (
          [ "GHC" </> "Parser.hs" | ghcSeries ghcFlavor >= Ghc90] ++
          [ "Parser.hs" | ghcSeries ghcFlavor < Ghc90]
         )
        )

  putStrLn "# Generating 'ghc/.parser-depends'..."
  putStrLn $ "\n\n# Running: " ++ cmd
  system_ cmd

  buf <- readFile' ".parser-depends"
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
      strippedModulePaths = foldl
        (\acc p -> map (replace (p ++ "/") "") acc)
        modulePaths
        (placeholderModulesDir : hsSrcDirs)
      -- Lastly, manipulate text like 'GHC/Exts/Heap/Constants.hs'
      -- into 'GHC.Exts.Heap.Constants'.
      modules = map (replace "/" "." . dropSuffix ".hs") strippedModulePaths
      -- The modules in this list elude being listed in
      -- '.parser-depends' but are required by ghc-lib-parser. We
      -- intervene and patch things up.
      extraModules =
        [ x | ghcSeries ghcFlavor >= Ghc94, x <- [ "GHC.Runtime.Interpreter", "GHCi.BinaryArray", "GHCi.BreakArray", "GHCi.ResolvedBCO", "GHC.Driver.Config.Parser" ] ] ++
        [ x | ghcSeries ghcFlavor >= Ghc92,  x <- [ "GHC.Driver.Config", "GHC.Parser.Errors.Ppr" ] ] ++
        [ if ghcSeries ghcFlavor >= Ghc90 then "GHC.Parser.Header" else "HeaderInfo", if ghcSeries ghcFlavor >= Ghc810 then "GHC.Hs.Dump" else "HsDumpAst" ]
  return $ nubSort (modules ++ extraModules)

applyPatchSystemSemaphore :: FilePath -> GhcFlavor -> IO ()
applyPatchSystemSemaphore patches ghcFlavor = do
  when (ghcSeries ghcFlavor == Ghc98) $ do
    -- https://gitlab.haskell.org/ghc/ghc/-/commit/5c8731244bc13a3d813d2a4d53b3188b28dc8355#774d88050336ef660c7a219fb06c480c2fc639bc

    -- Since Apr 2023 ghc depends on a newly created library -
    -- semaphore-compat. To keep building ghc-flavor 9.8 with ghc-9.4
    -- and ghc-9.6 series build compilers on all platforms we do as
    -- minimal an unwinding of the commit as we can such that ghc-lib
    -- itself avoids depending on semaphore-compat. Hopefully this
    -- patch will hold us over until the minimum build compiler
    -- version is known to ship with semaphore-compat at which point
    -- semaphore-compat can be added to the ghc-lib build deps and
    -- this patch removed.
    system_ $ "git apply " ++ (patches </> "5c8731244bc13a3d813d2a4d53b3188b28dc835-ghc_cabal_in.patch")
    system_ $ "git apply " ++ (patches </> "5c8731244bc13a3d813d2a4d53b3188b28dc835-GHC_Driver_MakeSem_hs.patch")
    system_ $ "git apply " ++ (patches </> "5c8731244bc13a3d813d2a4d53b3188b28dc835-GHC_Driver_Make_hs.patch")
    system_ $ "git apply " ++ (patches </> "5c8731244bc13a3d813d2a4d53b3188b28dc835-GHC_Driver_Session_hs.patch")
    system_ $ "git apply " ++ (patches </> "5c8731244bc13a3d813d2a4d53b3188b28dc835-GHC_Driver_Pipeline_LogQueue_hs.patch")

applyPatchTemplateHaskellCabal :: GhcFlavor -> IO ()
applyPatchTemplateHaskellCabal ghcFlavor = do
  when (ghcSeries ghcFlavor == Ghc94) $ do
    -- In
    -- https://gitlab.haskell.org/ghc/ghc/-/commit/b151b65ec469405dcf25f9358e7e99bcc8c2b3ac
    -- (2022/7/05) a temporary change is made to provide for vendoring
    -- filepath inside template-haskell. This breaks our simple cabal
    -- parsing so workaround while this situation exists.
    writeFile "libraries/template-haskell/template-haskell.cabal.in" .
      replace
        (unlines [
            "    if flag(vendor-filepath)"
          , "      other-modules:"
          , "        System.FilePath"
          , "        System.FilePath.Posix"
          , "        System.FilePath.Windows"
          , "      hs-source-dirs: ../filepath ."
          , "      default-extensions:"
          , "        ImplicitPrelude"
          , "    else"
          , "      build-depends: filepath"
          , "      hs-source-dirs: ."
          ])
        "        filepath" .
      replace
        (unlines [
            "    if flag(vendor-filepath)"
          , "      other-modules:"
          , "        System.FilePath"
          , "        System.FilePath.Posix"
          , "        System.FilePath.Windows"
          , "      hs-source-dirs: ./vendored-filepath ."
          , "      default-extensions:"
          , "        ImplicitPrelude"
          , "    else"
          , "      build-depends: filepath"
          , "      hs-source-dirs: ."
          ])
          "        filepath"
      =<< readFile' "libraries/template-haskell/template-haskell.cabal.in"

  when (ghcSeries ghcFlavor >= Ghc96) $ do
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
    writeFile "libraries/template-haskell/template-haskell.cabal.in" .
      replace
        (unlines [
            "    other-modules:"
          , "      System.FilePath"
          , "      System.FilePath.Posix"
          , "      System.FilePath.Windows"
          , "    hs-source-dirs: ./vendored-filepath ."
        ])
        (unlines[
            "    build-depends:"
          , "        filepath"
          , "    hs-source-dirs: ."
        ])
        =<< readFile' "libraries/template-haskell/template-haskell.cabal.in"

-- Avoid duplicate symbols with HSghc-heap (see issue
-- https://github.com/digital-asset/ghc-lib/issues/210).
applyPatchHeapClosures :: GhcFlavor -> IO ()
applyPatchHeapClosures _ = do
  writeFile "libraries/ghc-heap/cbits/HeapPrim.cmm" .
    replace
      "aToWordzh"
      "Ghclib_aToWordzh" .
    replace
      "reallyUnsafePtrEqualityUpToTag"
      "Ghclib_reallyUnsafePtrEqualityUpToTag"
    =<< readFile' "libraries/ghc-heap/cbits/HeapPrim.cmm"
  writeFile "libraries/ghc-heap/GHC/Exts/Heap/Closures.hs" .
    replace
      "\"aToWordzh\""
      "\"Ghclib_aToWordzh\"" .
    replace
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
  when (ghcSeries ghcFlavor < Ghc94) $ do
    renameFileRewriteSrcs "compiler" "HsVersions.h" ["compiler", stage0Compiler] [".hs", ".y", ".hsc"]

-- Rename 'DerivedConstants.h' to 'GhclibDerivedConstants.h'.
applyPatchDerivedConstants :: GhcFlavor -> IO ()
applyPatchDerivedConstants ghcFlavor =
    renameFileRewriteSrcs
      (if ghcSeries ghcFlavor >= Ghc94 then
        stage0Rts </> "include"
      else
        hadrianGeneratedRoot ghcFlavor
      )
      "DerivedConstants.h"
      (["compiler", stage0Compiler] ++ [stage0Rts | ghcSeries ghcFlavor >= Ghc94 ])
      [".hs", ".y", ".hsc"]

-- Selectively disable optimizations in some particular files so as
-- to reduce (user) compile times. The files we apply this to were
-- those identified as bottlenecks in the 2019 GSoC Hadrian speed
-- project.
applyPatchDisableCompileTimeOptimizations :: GhcFlavor -> IO ()
applyPatchDisableCompileTimeOptimizations ghcFlavor =
    let files
          | ghcSeries ghcFlavor >= Ghc90 = [ "compiler/GHC/Driver/Session.hs", "compiler/GHC/Hs.hs" ]
          | ghcSeries ghcFlavor >= Ghc810 = [ "compiler/main/DynFlags.hs", "compiler/GHC/Hs.hs" ]
          | otherwise            = [ "compiler/main/DynFlags.hs", "compiler/hsSyn/HsInstances.hs" ]
    in
      forM_ files $
        \file ->
          writeFile file .
          ("{-# OPTIONS_GHC -O0 #-}\n" ++)
          =<< readFile' file

applyPatchGHCiInfoTable :: GhcFlavor -> IO ()
applyPatchGHCiInfoTable ghcFlavor = do
  when(ghcFlavor == DaGhc881) $
    -- Drop references to RTS symbols in GHCi so we can build with GHC 9.
    -- These functions are never used since GHCi doesn’t work in ghc-lib anyway.
    writeFile infoTableHsc .
      replace
        (unlines
           [ "foreign import ccall unsafe \"allocateExec\""
           , "  _allocateExec :: CUInt -> Ptr (Ptr a) -> IO (Ptr a)"
           , ""
           , "foreign import ccall unsafe \"flushExec\""
           , "  _flushExec :: CUInt -> Ptr a -> IO ()"
           ])
        (unlines
           [ "_allocateExec :: CUInt -> Ptr (Ptr a) -> IO (Ptr a)"
           , "_allocateExec = error \"_allocateExec stub for ghc-lib\""
           , ""
           , "_flushExec :: CUInt -> Ptr a -> IO ()"
           , "_flushExec = error \"_flushExec stub for ghc-lib\""
           ])
      =<< readFile' infoTableHsc
  when(ghcSeries ghcFlavor >= Ghc92) $ do
    writeFile infoTableHsc .
      replace
        (unlines newExecConItblBefore)
        ("#if MIN_VERSION_rts(1,0,1)\n"   <>
           unlines newExecConItblBefore   <>
         "#else\n"                        <>
           unlines newExecConItblAfter    <>
         "#endif\n") .
      replace
        "fillExecBuffer :: CSize -> (Ptr a -> Ptr a -> IO ()) -> IO (Ptr a)\n"
        ("#if MIN_VERSION_rts(1,0,1)\n"                                           <>
           "fillExecBuffer :: CSize -> (Ptr a -> Ptr a -> IO ()) -> IO (Ptr a)\n" <>
         "#endif\n") .
      replace
        (if ghcSeries ghcFlavor >= Ghc94
           then
             "#error Sorry, rts versions <= 1.0 are not supported"
           else
             "#error hi"
        )
      (unlines [
           "foreign import ccall unsafe \"allocateExec\""
          , "  _allocateExec :: CUInt -> Ptr (Ptr a) -> IO (Ptr a)"
          , ""
          , "foreign import ccall unsafe \"flushExec\""
          , "  _flushExec :: CUInt -> Ptr a -> IO ()" ]
      )
      =<< readFile' infoTableHsc
  where
    infoTableHsc = "libraries/ghci/GHCi/InfoTable.hsc"

    newExecConItblBefore = [
        "newExecConItbl :: Bool -> StgInfoTable -> ByteString -> IO (FunPtr ())"
      , "newExecConItbl tables_next_to_code obj con_desc = do"
      , "    sz0 <- sizeOfEntryCode tables_next_to_code"
      , "    let lcon_desc = BS.length con_desc + 1{- null terminator -}"
      , "        -- SCARY"
      , "        -- This size represents the number of bytes in an StgConInfoTable."
      , "        sz = fromIntegral $ conInfoTableSizeB + sz0"
      , "            -- Note: we need to allocate the conDesc string next to the info"
      , "            -- table, because on a 64-bit platform we reference this string"
      , "            -- with a 32-bit offset relative to the info table, so if we"
      , "            -- allocated the string separately it might be out of range."
      , ""
      , "    ex_ptr <- fillExecBuffer (sz + fromIntegral lcon_desc) $ \\wr_ptr ex_ptr -> do"
      , "        let cinfo = StgConInfoTable { conDesc = ex_ptr `plusPtr` fromIntegral sz"
      , "                                    , infoTable = obj }"
      , "        pokeConItbl tables_next_to_code wr_ptr ex_ptr cinfo"
      , "        BS.useAsCStringLen con_desc $ \\(src, len) ->"
      , "            copyBytes (castPtr wr_ptr `plusPtr` fromIntegral sz) src len"
      , "        let null_off = fromIntegral sz + fromIntegral (BS.length con_desc)"
      , "        poke (castPtr wr_ptr `plusPtr` null_off) (0 :: Word8)"
      , ""
      , "    pure $ if tables_next_to_code"
      , "      then castPtrToFunPtr $ ex_ptr `plusPtr` conInfoTableSizeB"
      , "      else castPtrToFunPtr ex_ptr"
      ]

    newExecConItblAfter = [
        "newExecConItbl :: Bool -> StgInfoTable -> ByteString -> IO (FunPtr ())"
      , "newExecConItbl tables_next_to_code obj con_desc"
      , "   = alloca $ \\pcode -> do"
      , "        sz0 <- sizeOfEntryCode tables_next_to_code"
      , "        let lcon_desc = BS.length con_desc + 1{- null terminator -}"
      , "            -- SCARY"
      , "            -- This size represents the number of bytes in an StgConInfoTable."
      , "            sz = fromIntegral $ conInfoTableSizeB + sz0"
      , "               -- Note: we need to allocate the conDesc string next to the info"
      , "               -- table, because on a 64-bit platform we reference this string"
      , "               -- with a 32-bit offset relative to the info table, so if we"
      , "               -- allocated the string separately it might be out of range."
      , "        wr_ptr <- _allocateExec (sz + fromIntegral lcon_desc) pcode"
      , "        ex_ptr <- peek pcode"
      , "        let cinfo = StgConInfoTable { conDesc = ex_ptr `plusPtr` fromIntegral sz"
      , "                                    , infoTable = obj }"
      , "        pokeConItbl tables_next_to_code wr_ptr ex_ptr cinfo"
      , "        BS.useAsCStringLen con_desc $ \\(src, len) ->"
      , "            copyBytes (castPtr wr_ptr `plusPtr` fromIntegral sz) src len"
      , "        let null_off = fromIntegral sz + fromIntegral (BS.length con_desc)"
      , "        poke (castPtr wr_ptr `plusPtr` null_off) (0 :: Word8)"
      , "        _flushExec sz ex_ptr -- Cache flush (if needed)"
      , "        pure $ if tables_next_to_code"
      , "          then castPtrToFunPtr $ ex_ptr `plusPtr` conInfoTableSizeB"
      , "          else castPtrToFunPtr ex_ptr"
      ]

applyPatchGHCiMessage :: GhcFlavor -> IO ()
applyPatchGHCiMessage ghcFlavor =
  when (ghcSeries ghcFlavor >= Ghc92) $ do
    -- Synthesize a definition of MIN_VERSION_ghc_heap. If X.Y.Z is
    -- the current version, then MIN_VERSION_ghc_heap(a, b, c) is a
    -- test of whether X.Y.Z >= a.b.c (that is, ghc-heap X.Y.Z is at
    -- least a.b.c).
    versionText <-
      if ghcSeries ghcFlavor < Ghc94
        then
          readFile' (hadrianGeneratedRoot ghcFlavor </> "ghcversion.h")
        else
          readFile' "rts/include/ghcversion.h" -- now generated by ./configure
    let ls = lines versionText
        Just version = firstJust (stripPrefix "#define __GLASGOW_HASKELL__ ") ls
        Just patchLevel = firstJust (stripPrefix "#define __GLASGOW_HASKELL_PATCHLEVEL1__ ") ls
        major1Major2 = read @Int version
        minor        = read @Int patchLevel
        [x, y, z]    = map show [ major1Major2 `div` 100, major1Major2 `mod` 100, minor ]
        rs = [
            "#ifndef MIN_VERSION_ghc_heap"
          , "#define MIN_VERSION_ghc_heap(major1,major2,minor) (\\"
          , "  (major1) <  " ++ x ++ " || \\"
          , "  (major1) == " ++ x ++ " && (major2) <  " ++ y ++ " || \\"
          , "  (major1) == " ++ x ++ " && (major2) == " ++ y ++ " && (minor) <= " ++ z ++ ")"
          , "#endif /* MIN_VERSION_ghc_heap */"
          ]
    -- Write this definition before it's tested on.
    writeFile messageHs .
        replace
          "#if MIN_VERSION_ghc_heap(8,11,0)"
          (unlines rs <> "#if MIN_VERSION_ghc_heap(8,11,0)")
      =<< readFile' messageHs
  where
      messageHs = "libraries/ghci/GHCi/Message.hs"

applyPatchHaddockHs :: GhcFlavor -> IO ()
applyPatchHaddockHs ghcFlavor = do
  -- See https://github.com/ndmitchell/hlint/issues/1224
  when (ghcFlavor `elem` [Ghc901, Ghc902]) (
    writeFile haddockHs .
      replace
        "-- *"
        "-- -"
    =<< readFile' haddockHs
    )
  -- See https://github.com/digital-asset/ghc-lib/issues/344
  when (ghcSeries ghcFlavor == Ghc92) (
    writeFile ffiClosuresHs .
      replace
        "-- *"
        "-- -"
    =<< readFile' ffiClosuresHs
    )

  -- See https://github.com/digital-asset/ghc-lib/issues/391
  when (ghcFlavor `elem` [Ghc923, Ghc924, Ghc925, Ghc926, Ghc927]) (
    writeFile codeGenHs . replace "{- | debugIsOn -}"  ""
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
  when (ghcSeries ghcFlavor >= Ghc92) (
    writeFile asmHs .
      replace
        "#include \"rts/Bytecodes.h\""
        (unlines [
              "#include \"rts/Bytecodes.h\""
            , "#if __GLASGOW_HASKELL__ <= 901"
            , "#  define bci_RETURN_T          69"
            , "#  define bci_PUSH_ALTS_T       70"
            , "#endif" ])
    =<< readFile' asmHs )
    where
      asmHs = "compiler/GHC/ByteCode/Asm.hs"

-- Workaround lack of newer ghc-prim 12/3/2019
-- (https://gitlab.haskell.org/ghc/ghc/commit/705a16df02411ec2445c9a254396a93cabe559ef)
applyPatchGhcPrim :: GhcFlavor -> IO ()
applyPatchGhcPrim ghcFlavor = do
    let tysPrim =
          "compiler/" ++
          if ghcSeries ghcFlavor >= Ghc90
            then "GHC/Builtin/Types/Prim.hs"
            else "prelude/TysPrim.hs"
    when (ghcSeries ghcFlavor >= Ghc90) (
      writeFile tysPrim .
          replaceIfGhcPrim070Else 0
            "bcoPrimTyCon = pcPrimTyCon0 bcoPrimTyConName LiftedRep"
            "bcoPrimTyCon = pcPrimTyCon0 bcoPrimTyConName UnliftedRep" .
          replaceIfGhcPrim070Else 0
            "bcoPrimTyConName              = mkPrimTc (fsLit \"BCO\") bcoPrimTyConKey bcoPrimTyCon"
            "bcoPrimTyConName              = mkPrimTc (fsLit \"BCO#\") bcoPrimTyConKey bcoPrimTyCon"
        =<< readFile' tysPrim
        )

    let createBCO = "libraries/ghci/GHCi/CreateBCO.hs"
    when (ghcSeries ghcFlavor >= Ghc90) (
      writeFile createBCO .
          replace
              "{-# LANGUAGE RecordWildCards #-}"
              "{-# LANGUAGE RecordWildCards #-}\n{-# LANGUAGE CPP #-}" .
          replaceIfGhcPrim070Else 5
              "do linked_bco <- linkBCO' arr bco"
              "do BCO bco# <- linkBCO' arr bco" .
          replaceIfGhcPrim070Else 11
            "then return (HValue (unsafeCoerce linked_bco))\n           else case mkApUpd0# linked_bco of { (# final_bco #) ->"
            "then return (HValue (unsafeCoerce# bco#))\n           else case mkApUpd0# bco# of { (# final_bco #) ->" .
          replaceIfGhcPrim070Else 6
            "bco <- linkBCO' arr bco\n      writePtrsArrayBCO i bco marr"
            "BCO bco# <- linkBCO' arr bco\n      writePtrsArrayBCO i bco# marr" .
          replaceIfGhcPrim070Else 0
            "writePtrsArrayBCO :: Int -> BCO -> PtrsArr -> IO ()"
            "writePtrsArrayBCO :: Int -> BCO# -> PtrsArr -> IO ()" .
          replaceIfGhcPrim070Else  0
            "writePtrsArrayMBA :: Int -> MutableByteArray# s -> PtrsArr -> IO ()"
            "data BCO = BCO BCO#\nwritePtrsArrayMBA :: Int -> MutableByteArray# s -> PtrsArr -> IO ()" .
          replaceIfGhcPrim070Else 2
            "newBCO# instrs lits ptrs arity bitmap s"
            "case newBCO# instrs lits ptrs arity bitmap s of\n    (# s1, bco #) -> (# s1, BCO bco #)"
        =<< readFile' createBCO
        )
  where
    replaceIfGhcPrim070Else :: Int -> String -> String -> String -> String
    replaceIfGhcPrim070Else n s r = replace s (ifGhcPrim070Else n s r)

    ifGhcPrim070Else :: Int -> String -> String -> String
    ifGhcPrim070Else n s r =
      let indent n s = replicate n ' ' ++ s  in
      unlines ["\n#if MIN_VERSION_ghc_prim(0, 7, 0)", indent n s, "#else", indent n r , "#endif" ]

-- | Fix up these rts include paths. We don't ship rts headers since
-- we run ghc-lib using the RTS of the compiler we build with - we go
-- to the compiler installation for those.
applyPatchRtsIncludePaths :: GhcFlavor -> IO ()
applyPatchRtsIncludePaths ghcFlavor = do
  let files =
        [ "compiler/GHC/Runtime/Heap/Layout.hs" | ghcSeries ghcFlavor >= Ghc90 ] ++
        [ "compiler/cmm/SMRep.hs" | ghcSeries ghcFlavor < Ghc90 ] ++
        [ "compiler/GHC/StgToCmm/Layout.hs"  | ghcSeries ghcFlavor >= Ghc810 ] ++
        [ "compiler/codeGen/StgCmmLayout.hs" | ghcSeries ghcFlavor < Ghc810 ]
  forM_ files $
    \file ->
        writeFile file .
          replace
              "../includes/rts"
              "rts"
        =<< readFile' file

-- | Mangle exported C symbols to avoid collisions between the symbols
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
    let file = "compiler/cbits/genSym.c" in
        writeFile file .
        prefixSymbol genSym .
        prefixSymbol initGenSym
        =<< readFile' file
    let files
          | ghcSeries ghcFlavor >= Ghc90 = ("compiler/GHC/Types" </>) <$> [ "Unique/Supply.hs", "Unique.hs" ]
          | otherwise = [ "compiler/basicTypes/UniqSupply.hs" ]
    forM_ files $ \file ->
        writeFile file .
        prefixForeignImport genSym .
        prefixForeignImport initGenSym
        =<< readFile' file
    let cUtils
          | ghcSeries ghcFlavor >= Ghc90 = [ "compiler/cbits/cutils.c" ]
          | otherwise = [ "compiler/parser/cutils.c", "compiler/parser/cutils.h" ]
    forM_ cUtils $ \file ->
        writeFile file .
        prefixSymbol enableTimingStats .
        prefixSymbol setHeapSize
        =<< readFile' file
    let file
          | ghcSeries ghcFlavor >= Ghc90 = "compiler/GHC/Driver/Session.hs"
          | otherwise = "compiler/main/DynFlags.hs"
      in
        writeFile file .
        prefixForeignImport enableTimingStats .
        prefixForeignImport setHeapSize
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
  when (ghcSeries ghcFlavor < Ghc810) $
    forM_ [ "compiler/ghci/Linker.hs", "compiler/utils/FastString.hs", "compiler/main/DynFlags.hs" ] $
    \file ->
      (writeFile file . replace "STAGE >= 2" "0" . replace "STAGE < 2" "1")
      =<< readFile' file

-- When autoconf > 1.69 (brew has started installing version 1.71)
-- '_AC_PROG_CC_C99' is invalid but it appears 'AC_PROG_CC_C99' works
-- ok in its place (see
-- https://gitlab.haskell.org/ghc/ghc/-/issues/19189). Warning
-- messages suggest that in fact 'AC_PROG_CC_C99' should now be
-- spelled 'AC_PROG_CC' but I'm not going that far as the former is
-- how it currently is on HEAD.
applyPatchAclocal :: GhcFlavor -> IO ()
applyPatchAclocal ghcFlavor =
  when (ghcFlavor == Ghc901) $
    writeFile aclocalm4 .
      replace "_AC_PROG_CC_C99" "AC_PROG_CC_C99"
    =<< readFile' aclocalm4
  where aclocalm4 = "aclocal.m4"

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
    forM_ [ "libraries/ghc-heap/GHC/Exts/Heap/InfoTable.hsc", "libraries/ghc-heap/GHC/Exts/Heap/InfoTableProf.hsc" ] $
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
    writeFile cmmParse .
      replace
        "import GhcPrelude"
        "import GhcPrelude\nimport qualified Prelude"
    =<< readFile' cmmParse

-- | Patch Hadrian's Cabal.
applyPatchHadrianStackYaml :: GhcFlavor -> Maybe String -> IO ()
applyPatchHadrianStackYaml ghcFlavor resolver = do
  let hadrianStackYaml = "hadrian/stack.yaml"
  config <- Y.decodeFileThrow hadrianStackYaml
  -- [Note : GHC now depends on exceptions package]
  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  -- 'exceptions' is required by flavors >= ghc-9.0.1 but isn't in the
  -- compiler packages when the build compiler is of the 8.8 series so
  -- this makes sure it's there in that case.
  let deps = ["exceptions-0.10.4" | ghcSeries ghcFlavor == Ghc90] ++
        case parse (\cfg -> cfg .:? "extra-deps" .!= []) config of
          Success ls -> ls :: [Y.Value]
          Error msg -> error msg
 -- Build hadrian (and any artifacts we generate via hadrian e.g.
 -- Parser.hs) as quickly as possible.
  let opts = HMS.insert "$everything" "-O0 -j" $
        case parse (\cfg -> cfg .:? "ghc-options" .!= HMS.empty) config of
          Success os -> os :: HMS.HashMap T.Text Y.Value
          Error msg -> error msg
  let config' =
        HMS.insert "extra-deps" (toJSON deps)
          (HMS.insert "ghc-options" (toJSON opts)
#if !MIN_VERSION_aeson(2, 0, 2)
                                     config
#else
                                     (toHashMap config)
#endif
          )
    -- [Note: Hack "ghc-X.X.X does not compile with ghc XXX"]
    -- ------------------------------------------------------
    -- See for example
    -- https://gitlab.haskell.org/ghc/ghc/-/issues/21633 &
    -- https://gitlab.haskell.org/ghc/ghc/-/issues/21634.
    --
    -- The idea is to replace the resolver with whatever is prevailing
    -- (or ghc-9.2.5 if that's not possible).
      resolverDefault = "lts-20.10" -- ghc-9.2.5
      -- The resolver has to curate packages so resolvers of the form
      -- ghc-x.y.z won't do.
      resolver' = case fromMaybe resolverDefault resolver of
        r | "ghc-" `isPrefixOf` r -> resolverDefault
        r -> r
      -- This is still an issue with 9.6.1 (resolver in
      -- hadrian/stack.yaml is a 9.0.2 resolver i.e. not recent enough
      -- to build GHC so causes a configure error).
      config'' = if ghcSeries ghcFlavor < Ghc94
                     then config'
                     else
                         HMS.insert "allow-newer" (toJSON True)
                           (HMS.update (\_ -> Just (toJSON resolver')) "resolver" config')
  Y.encodeFile hadrianStackYaml config''

-- | Data type representing an approximately parsed Cabal file.
data Cabal = Cabal
    { cabalDir :: FilePath -- the directory this file exists in
    , cabalFields :: [(String, [String])] -- the key/value pairs it contains
    }

-- | Given a file, produce the key/value pairs it contains
-- (approximate but good enough).
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

-- | Ask a Cabal file for a field.
askCabalField :: Cabal -> String -> [String]
askCabalField cbl x = concatMap snd $ filter ((==) x . fst) $ cabalFields cbl

-- | Ask a Cabal file for files, relative to the underlying Cabal
-- file.
askCabalFiles :: Cabal -> String -> [String]
askCabalFiles cbl x = map (cabalDir cbl </>) $ askCabalField cbl x

-- | Harvest a field from a set of Cabal files (such as
-- 'exposed-modules').
askField :: [Cabal] -> String -> [String]
askField from x = nubSort $ concatMap (`askCabalField` x) from

-- | Harvest a set of files from a set of Cabal files (such as the
-- 'hs-sourc-dirs').
askFiles :: [Cabal] -> String -> [String]
askFiles from x = nubSort $ concatMap (`askCabalFiles` x) from

-- | Some often used string manipulation utilities.
indent :: [String] -> [String]
indent = map ("    " ++)
indent2 :: [String] -> [String]
indent2 = indent . indent
withCommas :: [String] -> [String]
withCommas ms =
  let ms' = reverse ms in
    reverse (head ms' : map (++ ",") (tail ms'))

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
    DaGhc881  -> "base >= 4.11 && < 4.16" -- unlike upstream GHC 8.8, the DA fork does work with ghc-8.10.1 and ghc-9.0.2
    Ghc881    -> "base >= 4.11 && < 4.14" -- [ghc-8.4.4, ghc-8.10.1)
    Ghc882    -> "base >= 4.11 && < 4.14"
    Ghc883    -> "base >= 4.11 && < 4.14"
    Ghc884    -> "base >= 4.11 && < 4.14"

    Ghc8101   -> "base >= 4.12 && < 4.15" -- [ghc-8.6.5, ghc-9.0.1)
    Ghc8102   -> "base >= 4.12 && < 4.15"
    Ghc8103   -> "base >= 4.12 && < 4.15"
    Ghc8104   -> "base >= 4.12 && < 4.15"
    Ghc8105   -> "base >= 4.12 && < 4.15"
    Ghc8106   -> "base >= 4.12 && < 4.15"
    Ghc8107   -> "base >= 4.12 && < 4.15"

    Ghc901    -> "base >= 4.13 && < 4.16" -- [ghc-8.8.1, ghc-9.2.1)
    Ghc902    -> "base >= 4.13 && < 4.16" -- [ghc-8.8.1, ghc-9.2.1)

    -- ghc-9.2.1, base-4.16.0.0
    -- ghc-9.2.2, base-4.16.1.0
    -- ghc-9.2.2, base-4.16.2.0
    -- ghc-9.2.4, base-4.16.3.0
    -- ghc-9.2.5, ghc-9.2.6, ghc-9.2.7 ship with base-4.16.4.0
    Ghc921    -> "base >= 4.14 && < 4.16.1" -- [ghc-8.10.1, ghc-9.2.2)
    Ghc922    -> "base >= 4.14 && < 4.16.2" -- [ghc-8.10.1, ghc-9.2.3)
    Ghc923    -> "base >= 4.14 && < 4.16.3" -- [ghc-8.10.1, ghc-9.2.4)
    Ghc924    -> "base >= 4.14 && < 4.16.4" -- [ghc-8.10.1, ghc-9.2.5)
    Ghc925    -> "base >= 4.14 && < 4.17" -- [ghc-8.10.1, ghc-9.4.1)
    Ghc926    -> "base >= 4.14 && < 4.17" -- [ghc-8.10.1, ghc-9.4.1)
    Ghc927    -> "base >= 4.14 && < 4.17" -- [ghc-8.10.1, ghc-9.4.1)

    -- ghc-9.4.1, ghc-9.4.2, ghc-9.4.3, ghc-9.4.4 all ship with
    -- base-4.17.0.0, ghc-9.4.5 has base-4.17.1.0
    Ghc941   -> "base >= 4.15 && < 4.18" -- [ghc-9.0.1, ghc-9.6.1)
    Ghc942   -> "base >= 4.15 && < 4.18" -- [ghc-9.0.1, ghc-9.6.1)
    Ghc943   -> "base >= 4.15 && < 4.18" -- [ghc-9.0.1, ghc-9.6.1)
    Ghc944   -> "base >= 4.15 && < 4.18" -- [ghc-9.0.1, ghc-9.6.1)
    Ghc945   -> "base >= 4.15 && < 4.18" -- [ghc-9.0.1, ghc-9.6.1)

    -- require bytestring >= 0.11.3 which rules out ghc-9.2.1
    Ghc961   -> "base >= 4.16.1 && < 4.19" -- [ghc-9.2.2, ghc-9.7.1)

    GhcMaster -- e.g. "9.7.20230119"
              -- (c.f. 'rts/include/ghc-version.h'')
      -> "base >= 4.17.0.0 && < 4.20" -- [ghc-9.4.1, ghc-9.8.1)

-- | Common build dependencies.
commonBuildDepends :: GhcFlavor -> [String]
commonBuildDepends ghcFlavor =
  base ++ specific ++ conditional ++ shared
  where
    -- base
    base = [ baseBounds ghcFlavor ]
    specific
       | ghcSeries ghcFlavor >= Ghc96  = [
           "ghc-prim > 0.2 && < 0.11"
         , "bytestring >= 0.11.3 && < 0.12"
         , "time >= 1.4 && < 1.13"
         ]
       | ghcSeries ghcFlavor >= Ghc94  = [
           "ghc-prim > 0.2 && < 0.10"
         , "bytestring >= 0.10 && < 0.12"
         , "time >= 1.4 && < 1.13"
         ]
        | ghcSeries ghcFlavor >= Ghc92 = [
            "ghc-prim > 0.2 && < 0.9"
          , "bytestring >= 0.9 && < 0.12"
          , "time >= 1.4 && < 1.12"
          ]
        | otherwise = [
            "ghc-prim > 0.2 && < 0.8"
          , "bytestring >= 0.9 && < 0.11"
          , "time >= 1.4 && < 1.10"
          ]
    conditional
        | ghcSeries ghcFlavor >= Ghc90 = [
            "exceptions == 0.10.*"
          , "parsec"
          ]
        | otherwise = []
    -- shared for all flavors
    shared = [
        "containers >= 0.5 && < 0.7"
      , "binary == 0.8.*"
      , "filepath >= 1 && < 1.5"
      , "directory >= 1 && < 1.4"
      , "array >= 0.1 && < 0.6"
      , "deepseq >= 1.4 && < 1.5"
      , "pretty == 1.1.*"
      , "transformers >= 0.5 && < 0.7"
      , "process >= 1 && < 1.7"
      ]

ghcLibParserBuildDepends :: GhcFlavor -> [String]
ghcLibParserBuildDepends  = commonBuildDepends

ghcLibBuildDepends :: GhcFlavor -> [String]
ghcLibBuildDepends ghcFlavor =
  commonBuildDepends ghcFlavor ++
  [ "stm" | ghcSeries ghcFlavor >= Ghc94 ] ++
  [ "rts"
  , "hpc == 0.6.*"
  , "ghc-lib-parser"  -- we rely on this being last (in CI.hs:
                      -- 'patchConstraints')!
  ]

-- | This utility factored out to avoid repetion.
libBinParserModules :: GhcFlavor -> IO ([Cabal], [Cabal], [String])
libBinParserModules ghcFlavor = do
    lib <- mapM readCabalFile cabalFileLibraries
    bin <- readCabalFile cabalFileBinary
    parserModules <- calcParserModules ghcFlavor
    return (lib, [bin], parserModules)

-- | Produces a ghc-lib Cabal file.
generateGhcLibCabal :: GhcFlavor -> [String] -> IO ()
generateGhcLibCabal ghcFlavor customCppOpts = do
    -- Compute the list of modules to be compiled. The rest are parser
    -- modules re-exported from ghc-lib-parser.
    (lib, _bin, parserModules) <- libBinParserModules ghcFlavor
    let nonParserModules =
          Set.toList (Set.difference
          (Set.fromList (askField lib "exposed-modules:" ))
          (Set.fromList parserModules))

    writeFile "ghc-lib.cabal" $ unlines $ map trimEnd $
        [ "cabal-version: 2.0"
        , "build-type: Simple"
        , "name: ghc-lib"
        , "version: 0.1.0"
        , "license: BSD3"
        , "license-file: LICENSE"
        , "category: Development"
        , "author: The GHC Team and Digital Asset"
        , "maintainer: Digital Asset"
        , "synopsis: The GHC API, decoupled from GHC versions"
        , "description: A package equivalent to the @ghc@ package, but which can be loaded on many compiler versions."
        , "homepage: https://github.com/digital-asset/ghc-lib"
        , "bug-reports: https://github.com/digital-asset/ghc-lib/issues"
        , "data-dir: " ++ dataDir
        , "data-files:"] ++ indent (dataFiles ghcFlavor) ++
        [ "extra-source-files:"] ++ indent (performExtraFilesSubstitutions ghcFlavor ghcLibExtraFiles) ++
        [ "source-repository head"
        , "    type: git"
        , "    location: git@github.com:digital-asset/ghc-lib.git"
        ] ++
        [ "flag threaded-rts"
        , "    default: True"
        , "    manual: True"
        , "    description: Pass -DTHREADED_RTS to the C toolchain"
        ] ++
        [ "library"
        , "    default-language:   Haskell2010"
        , "    exposed: False"
        , "    include-dirs:"
        ] ++ indent2 (ghcLibIncludeDirs ghcFlavor) ++
        [ "    if flag(threaded-rts)"
        , "        ghc-options: -fobject-code -package=ghc-boot-th -optc-DTHREADED_RTS"
        , "        cc-options: -DTHREADED_RTS"
        , "        cpp-options: -DTHREADED_RTS " <> generateCppOpts ghcFlavor customCppOpts
        , "    else"
        , "        ghc-options: -fobject-code -package=ghc-boot-th"
        , "        cpp-options: " <> generateCppOpts ghcFlavor customCppOpts
        ] ++
        [ "    if !os(windows)"
        , "        build-depends: unix"
        , "    else"
        , "        build-depends: Win32"
        , "    build-depends:" ] ++
        indent2 (withCommas (ghcLibBuildDepends ghcFlavor))++
        [ "    build-tool-depends: alex:alex >= 3.1, happy:happy >= 1.19.4"
        , "    other-extensions:" ] ++ indent2 (askField lib "other-extensions:") ++
        [ "    default-extensions:" ] ++ indent2 (askField lib "default-extensions:") ++
        [ "    hs-source-dirs:" ] ++
        indent2 (ghcLibHsSrcDirs ghcFlavor lib) ++
        [ "    autogen-modules:"
        , "        Paths_ghc_lib"
        ] ++
        [ "    reexported-modules:" ] ++
        withCommas (indent2 $ nubSort parserModules) ++
        [ "    exposed-modules:"
        , "        Paths_ghc_lib"
        ] ++
        indent2 (nubSort nonParserModules)
    putStrLn "# Generating 'ghc-lib.cabal'... Done!"

generateCppOpts :: GhcFlavor -> [String] -> String
generateCppOpts ghcFlavor customCppOpts =
  unwords $ [
      ghcStageDef ghcFlavor
    , ghciDef ghcFlavor
    , ghcInGhciDef ghcFlavor
    ] ++
  customCppOpts
  where
    ghciDef, ghcInGhciDef, ghcStageDef :: GhcFlavor -> String
    ghciDef ghcFlavor = if ghcSeries ghcFlavor > Ghc810 then "" else "-DGHCI"
    ghcInGhciDef = \case f | ghcSeries f >= Ghc92 -> ""; _ -> "-DGHC_IN_GHCI"
    ghcStageDef = \case f | ghcSeries f >= Ghc810 -> ""; _ -> "-DSTAGE=2"

-- Perform a set of specific substitutions on the given list of files.
performExtraFilesSubstitutions :: GhcFlavor -> (GhcFlavor -> [FilePath]) -> [FilePath]
performExtraFilesSubstitutions ghcFlavor files =
  foldl' sub (files ghcFlavor) $
      [ ("rts/include/ghcversion.h", Nothing) | ghcSeries ghcFlavor >= Ghc94 ] ++
      [ (hadrianGeneratedRoot ghcFlavor </> "ghcversion.h", Nothing) | ghcSeries ghcFlavor < Ghc94 ] ++
      [  ((stage0Rts </> "include") </> "DerivedConstants.h", Just $ (stage0Rts </> "include") </> "GhclibDerivedConstants.h") | ghcSeries ghcFlavor >= Ghc94] ++
      [ (hadrianGeneratedRoot ghcFlavor </> "DerivedConstants.h", Just $ hadrianGeneratedRoot ghcFlavor </> "GhclibDerivedConstants.h") | ghcSeries ghcFlavor < Ghc94 ] ++
      [("compiler" </> "HsVersions.h", Just $ "compiler" </> "GhclibHsVersions.h") | ghcSeries ghcFlavor < Ghc94 ]
  where
    sub :: Eq a => [a] -> (a, Maybe a) -> [a]
    sub xs (s, r) = replace [s] (maybeToList r) xs

-- | Produces a ghc-lib-parser Cabal file.
generateGhcLibParserCabal :: GhcFlavor -> [String] -> IO ()
generateGhcLibParserCabal ghcFlavor customCppOpts = do
    (lib, _bin, parserModules) <- libBinParserModules ghcFlavor
    writeFile "ghc-lib-parser.cabal" $ unlines $ map trimEnd $
        [ "cabal-version: 2.0"
        , "build-type: Simple"
        , "name: ghc-lib-parser"
        , "version: 0.1.0"
        , "license: BSD3"
        , "license-file: LICENSE"
        , "category: Development"
        , "author: The GHC Team and Digital Asset"
        , "maintainer: Digital Asset"
        , "synopsis: The GHC API, decoupled from GHC versions"
        , "description: A package equivalent to the @ghc@ package, but which can be loaded on many compiler versions."
        , "homepage: https://github.com/digital-asset/ghc-lib"
        , "bug-reports: https://github.com/digital-asset/ghc-lib/issues"
        , "data-dir: " ++ dataDir
        , "data-files:"
        ] ++ indent (dataFiles ghcFlavor) ++
        [ "extra-source-files:"] ++ indent (performExtraFilesSubstitutions ghcFlavor ghcLibParserExtraFiles) ++
        [ "source-repository head"
        , "    type: git"
        , "    location: git@github.com:digital-asset/ghc-lib.git"
        ] ++
        [ "flag threaded-rts"
        , "  default: True"
        , "  manual: True"
        , "  description: Pass -DTHREADED_RTS to the C toolchain"
        ] ++
        [ "library"
        , "    default-language:   Haskell2010"
        , "    exposed: False"
        , "    include-dirs:"] ++ indent2 (ghcLibParserIncludeDirs ghcFlavor) ++
        [ "    if flag(threaded-rts)"
        , "        ghc-options: -fobject-code -package=ghc-boot-th -optc-DTHREADED_RTS"
        , "        cc-options: -DTHREADED_RTS"
        , "        cpp-options: -DTHREADED_RTS " <> generateCppOpts ghcFlavor customCppOpts
        , "    else"
        , "        ghc-options: -fobject-code -package=ghc-boot-th"
        , "        cpp-options: " <> generateCppOpts ghcFlavor customCppOpts
        ] ++
        [ "    if !os(windows)"
        , "        build-depends: unix"
        , "    else"
        , "        build-depends: Win32"
        , "    build-depends:" ] ++
        indent2 (withCommas (ghcLibParserBuildDepends ghcFlavor)) ++
        [ "    build-tool-depends: alex:alex >= 3.1, happy:happy >= 1.19.4"
        , "    other-extensions:" ] ++ indent2 (askField lib "other-extensions:") ++
        [ "    default-extensions:" ] ++ indent2 (askField lib "default-extensions:") ++
        [ "    c-sources:" ] ++
        -- List CMM sources here. Go figure! (see
        -- https://twitter.com/smdiehl/status/1231958702141431808?s=20
        -- and
        -- https://gist.github.com/sdiehl/0491596cd7faaf95503e4b7066cffe62).
        indent2 [ "libraries/ghc-heap/cbits/HeapPrim.cmm" ] ++
        -- We hardcode these because the inclusion of 'keepCAFsForGHCi'
        -- causes issues in ghci see
        -- https://github.com/digital-asset/ghc-lib/issues/27
        indent2 [ "compiler/cbits/genSym.c" ] ++
        indent2 [ "compiler/cbits/cutils.c" | ghcSeries ghcFlavor >= Ghc90 ] ++
        indent2 [ "compiler/parser/cutils.c" | ghcSeries ghcFlavor < Ghc90 ] ++
        indent2 [ "compiler/cbits/keepCAFsForGHCi.c" | ghcFlavor `elem` [Ghc926, Ghc927, Ghc945] || ghcSeries ghcFlavor >= Ghc96 ] ++
        [ "    hs-source-dirs:" ] ++
        indent2 (ghcLibParserHsSrcDirs False ghcFlavor lib) ++
        [ "    autogen-modules:" ] ++
        indent2 [ x | ghcSeries ghcFlavor >= Ghc90, x <- [ "GHC.Parser.Lexer", "GHC.Parser" ] ] ++
        indent2 [ x |  ghcSeries ghcFlavor < Ghc90, x <- [ "Lexer", "Parser" ] ] ++
        ["    exposed-modules:" ] ++ indent2 parserModules
    putStrLn "# Generating 'ghc-lib-parser.cabal'... Done!"

-- | Run Hadrian to build the things that the Cabal files need.
generatePrerequisites :: GhcFlavor -> IO ()
generatePrerequisites ghcFlavor = do
  when (ghcSeries ghcFlavor < Ghc810) (
    -- Workaround a Windows bug present in at least 8.4.3. See
    -- http://haskell.1045720.n5.nabble.com/msys-woes-td5898334.html
    writeFile "./mk/get-win32-tarballs.sh" .
      replace
        "$curl_cmd || echo \"Checking repo.msys2.org instead of Haskell.org...\" && $curl_cmd_bnk || {"
        "$curl_cmd || (echo \"Checking repo.msys2.org instead of Haskell.org...\" && $curl_cmd_bnk) || {"
      =<< readFile' "./mk/get-win32-tarballs.sh"
    )

  -- If building happy in the next step, the configure it does
  -- requires some versions of alex and happy pre-exist. We make sure
  -- of this in CI.hs.
  system_ "stack --stack-yaml hadrian/stack.yaml build --only-dependencies"
  system_ "stack --stack-yaml hadrian/stack.yaml exec -- bash -c ./boot"
  system_ "stack --stack-yaml hadrian/stack.yaml exec -- bash -c \"./configure --enable-tarballs-autodownload\""
  withCurrentDirectory "hadrian" $ do
    -- No need to specify a stack.yaml here, we are in the hadrian
    -- directory itself.
    system_ "stack build --no-library-profiling"
    system_ $ unwords $ [
            "stack exec hadrian --"
          , "--directory=.."
          , "--build-root=ghc-lib"
        ] ++
        ["--bignum=native" | ghcSeries ghcFlavor >= Ghc90] ++
        ["--integer-simple" | ghcSeries ghcFlavor < Ghc90] ++
        ghcLibParserExtraFiles ghcFlavor ++ map (dataDir </>) (dataFiles ghcFlavor)

-- | Given an Hsc, Alex, or Happy file, generate a placeholder module
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
      if "module " `T.isPrefixOf` l then
        case T.words l of
          _module : name : _ -> pure $ T.takeWhile (/= '(') name
          _ -> fail $ "Cannot parse module name of " ++ m
      else
        parseModuleName h

    parseModuleImports :: Handle -> [T.Text] -> IO [T.Text]
    parseModuleImports h acc = handleEof acc $ do
        l <- T.hGetLine h
        acc <- if "import " `T.isPrefixOf` l then
                 case T.words l of
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
    extraImports ".x" = [ "Data.Array", "Data.Array.Base", "GHC.Exts" ]  -- Alex adds these imports
    extraImports ".y" = [ "Data.Array", "GHC.Exts" ]  -- Happy adds these imports
    extraImports _ = []

genPlaceholderModules :: FilePath -> IO ()
genPlaceholderModules = loop
  where
    loop fp = do
      isDir <- doesDirectoryExist fp
      if isDir then do
        contents <- listDirectory fp
        mapM_ (loop . (fp </>)) contents
      else when (takeExtension fp `elem` [".x", ".y", ".hsc"]) $ genPlaceholderModule fp
