-- Copyright (c) 2019-2020, Digital Asset (Switzerland) GmbH and/or
-- its affiliates. All rights reserved.  SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

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
  , generatePrerequisites
  , mangleCSymbols
  , generateGhcLibCabal
  , generateGhcLibParserCabal
) where

import Control.Monad
import System.Process.Extra
import System.FilePath hiding ((</>), normalise, dropTrailingPathSeparator)
import System.FilePath.Posix((</>), normalise, dropTrailingPathSeparator) -- Make sure we generate / on all platforms.
import System.Directory.Extra
import System.IO.Extra
import Data.List.Extra hiding (find)
import Data.Char
import Data.Maybe
import Data.Ord
import qualified Data.Set as Set

import qualified Data.Text as T
import Data.Aeson.Types(parse, Result(..))
import qualified Data.Yaml as Y
import Data.Yaml (ToJSON(..), (.:?), (.!=))
import qualified Data.HashMap.Strict as HMS

import GhclibgenOpts

-- Constants.

-- | Cabal files from libraries inside GHC that are merged together.
cabalFileLibraries :: [FilePath]
cabalFileLibraries =
    [ "libraries/template-haskell/template-haskell.cabal"
    , "libraries/ghc-heap/ghc-heap.cabal"
    , "libraries/ghc-boot-th/ghc-boot-th.cabal"
    , "libraries/ghc-boot/ghc-boot.cabal"
    , "libraries/ghci/ghci.cabal"
    , "compiler/ghc.cabal"
    ]

-- | C-preprocessor "include dirs" for 'ghc-lib-parser'.
ghcLibParserIncludeDirs :: GhcFlavor -> [FilePath]
ghcLibParserIncludeDirs ghcFlavor =
  -- For
  (if ghcFlavor == GhcMaster
  then [ "rts/include" ]  -- ghcconfig.h
  else [ "includes" ]) ++ -- ghcconfig.h, MachDeps.h, MachRegs.h, CodeGen.Platform.hs
  -- Others
  [ hadrianGeneratedRoot ghcFlavor, stage0Compiler, "compiler" ] ++
  [ "compiler/utils" | ghcFlavor < Ghc8101 ]

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
        -- This next conditional just smooths over a 'master'
        -- vs. '8.8.1' branch difference (relating to a file
        -- 'GhcVersion.hs' IIRC).
        [ stage0GhcBoot | ghcFlavor >= Ghc8101 ] ++
        map takeDirectory cabalFileLibraries ++
        map (dropTrailingPathSeparator . normalise) (askFiles lib "hs-source-dirs:")

      excludes = Set.fromList $
        map ("compiler" </>) (
          [ "codeGen"
          , "hieFile"
          , "llvmGen"
          , "rename"
          , "stgSyn"
          , "stranal"
          ] ++
          [ d | ghcFlavor >= Ghc901, d <- [ "typecheck", "specialise", "cmm" ] ] ++
          [ "nativeGen" | ghcFlavor < Ghc901 ] ++ -- Since 2020-01-04. See https://gitlab.haskell.org/ghc/ghc/commit/d561c8f6244f8280a2483e8753c38e39d34c1f01.
          [ "deSugar" | ghcFlavor >= Ghc8101 && not forParserDepends ]
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
        [ stage0GhcBoot | ghcFlavor >= Ghc8101 ] ++ -- 'GHC.Platform' is in 'ghc-lib-parser', 'GHC.Platform.Host' is not.
        map takeDirectory cabalFileLibraries ++
        map (dropTrailingPathSeparator . normalise) (askFiles lib "hs-source-dirs:")
      excludes = Set.fromList $
        [ "compiler/basicTypes"
        , "compiler/parser"
        , "compiler/types"
        , "libraries/ghc-boot-th"
        , "libraries/ghc-heap"
        ] ++
        [ "compiler/cmm" | ghcFlavor >= Ghc901 ]

  in sortDiffListByLength all excludes -- Not so important. Here for symmetry with 'ghcLibParserHsSrcDirs' I think.

-- | Cabal file for the GHC binary.
cabalFileBinary :: FilePath
cabalFileBinary = "ghc/ghc-bin.cabal"

-- We set the hadrian build root to 'ghc-lib'. These constants appear
-- so frequently it's convenient to alias them for syntactic brevity.
ghcLibGeneratedPath :: FilePath
ghcLibGeneratedPath = "ghc-lib/generated"
stage0Root, stage0Compiler, stage0Libraries, stage0Lib :: FilePath
stage0GhcHeap, stage0GhcBoot, stage0Ghci :: FilePath
stage0Root = "ghc-lib/stage0"
stage0Lib = stage0Root </> "lib"
stage0Libraries = stage0Root </> "libraries"
stage0Compiler = stage0Root </> "compiler/build"
stage0GhcBoot = stage0Libraries </> "ghc-boot/build"
stage0GhcHeap = stage0Libraries </> "ghc-heap/build"
stage0Ghci = stage0Libraries </> "ghci/build"

-- | Sources generated by Hadrian are written under this directory.
hadrianGeneratedRoot :: GhcFlavor -> FilePath
hadrianGeneratedRoot = \case
  GhcMaster -> stage0Lib
  Ghc921 -> stage0Lib
  Ghc901 -> stage0Lib
  Ghc8105 -> stage0Lib
  Ghc8104 -> stage0Lib
  Ghc8103 -> stage0Lib
  Ghc8102 -> stage0Lib
  Ghc8101 -> stage0Lib
  _ -> ghcLibGeneratedPath

-- |'dataDir' is the directory cabal looks for data files to install,
-- relative to the source directory.
dataDir :: FilePath
dataDir = stage0Lib

-- |'dataFiles' is a list of files to be installed for run-time use by
-- the package.
dataFiles :: GhcFlavor -> [FilePath]
dataFiles ghcFlavor =
    -- From ghc/ghc.mk: "The GHC programs need to depend on all the
    -- helper programs they might call and the settings files they
    -- use."
    [ "settings"
    , "llvm-targets"
    , "llvm-passes"
    ] ++
    -- Since 2021-04-10 a platformConstants file is no longer a thing.
    -- See
    -- (https://gitlab.haskell.org/ghc/ghc/-/commit/2cdc95f9c068421a55c634933ab2d8596eb992fb).
    [ "platformConstants" | ghcFlavor <= Ghc921 ]

-- | See 'hadrian/src/Rules/Generate.hs'.

includesDependencies :: GhcFlavor -> [FilePath]
includesDependencies ghcFlavor =
  map (hadrianGeneratedRoot ghcFlavor </>)
    [ "ghcautoconf.h", "ghcplatform.h", "ghcversion.h" ]

derivedConstantsDependencies :: GhcFlavor -> [FilePath]
derivedConstantsDependencies ghcFlavor =
   map (hadrianGeneratedRoot ghcFlavor </>)
     ("DerivedConstants.h"
      : [ x | ghcFlavor <= Ghc901
          , x <- [ "GHCConstantsHaskellExports.hs"
                 , "GHCConstantsHaskellWrappers.hs"
                 , "GHCConstantsHaskellType.hs"
                 ]
        ]
     )

compilerDependencies :: GhcFlavor -> [FilePath]
compilerDependencies ghcFlavor =
  map (stage0Compiler </>) $
    [ "primop-can-fail.hs-incl"
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
    [ "primop-docs.hs-incl" | ghcFlavor >= Ghc901 ]

platformH :: GhcFlavor -> [FilePath]
platformH ghcFlavor =
  [ stage0Compiler </> "ghc_boot_platform.h" | ghcFlavor < Ghc8101 ]

packageCode :: GhcFlavor -> [FilePath]
packageCode ghcFlavor =
  [ stage0Compiler </> "Config.hs" | ghcFlavor < Ghc901 ] ++
  [ stage0GhcBoot  </> "GHC/Version.hs" | ghcFlavor >= Ghc8101 ] ++
  [ stage0Compiler </> "GHC/Settings/Config.hs" | ghcFlavor >= Ghc901 ] ++
  [ stage0Compiler </> "GHC/Platform/Constants.hs" | ghcFlavor >= Ghc921 ]

fingerprint :: GhcFlavor -> [FilePath]
fingerprint ghcFlavor = [ stage0Compiler </> "Fingerprint.hs" | ghcFlavor < Ghc8101 ]

-- | The C headers shipped with ghc-lib. These globs get glommed onto
-- the 'extraFiles' above as 'extra-source-files'.
cHeaders :: GhcFlavor -> [String]
cHeaders ghcFlavor =
  (if ghcFlavor == GhcMaster
  then
    [ "rts/include/ghcconfig.h"
    , "compiler/MachDeps.h"
    , "compiler/MachRegs.h"
    , "compiler/CodeGen.Platform.h"
    , "compiler/Bytecodes.h"
    , "compiler/ClosureTypes.h"
    , "compiler/FunTypes.h"
    , "compiler/Unique.h"
    ]
  else
    [ "includes/MachDeps.h"
    , "includes/stg/MachRegs.h"
    , "includes/CodeGen.Platform.hs"
    , "compiler/Unique.h"
    ]) ++
  [ "compiler/HsVersions.h" | ghcFlavor <= Ghc921] ++
  [ f | ghcFlavor < Ghc8101, f <- [ "compiler/nativeGen/NCG.h", "compiler/utils/md5.h"] ]

-- | We generate the parser and lexer and ship those rather than their
-- sources.
generatedParser :: GhcFlavor -> [FilePath]
generatedParser ghcFlavor =
  map (stage0Compiler </>)
    ( if ghcFlavor >= Ghc901
        then [ "GHC/Parser.hs", "GHC/Parser/Lexer.hs" ]
        else [ "Parser.hs", "Lexer.hs" ]
    )

-- | Cabal "extra-source-files" files for ghc-lib-parser.
ghcLibParserExtraFiles :: GhcFlavor -> [FilePath]
ghcLibParserExtraFiles ghcFlavor =
      includesDependencies ghcFlavor ++
      derivedConstantsDependencies ghcFlavor ++
      compilerDependencies ghcFlavor ++
      platformH ghcFlavor ++
      fingerprint ghcFlavor ++
      packageCode ghcFlavor ++
      generatedParser ghcFlavor ++
      cHeaders ghcFlavor

-- | Cabal "extra-source-files" for ghc-lib.
ghcLibExtraFiles :: GhcFlavor -> [FilePath]
ghcLibExtraFiles ghcFlavor =
      includesDependencies ghcFlavor ++
      derivedConstantsDependencies ghcFlavor ++
      compilerDependencies ghcFlavor ++
      platformH ghcFlavor ++
      fingerprint ghcFlavor ++
      cHeaders ghcFlavor

-- | Calculate via `ghc -M` the list of modules that are required for
-- 'ghc-lib-parser'.
calcParserModules :: GhcFlavor -> IO [String]
calcParserModules ghcFlavor = do
  lib <- mapM readCabalFile cabalFileLibraries
  let includeDirs = map ("-I" ++ ) (ghcLibParserIncludeDirs ghcFlavor)
      hsSrcDirs = ghcLibParserHsSrcDirs True ghcFlavor lib
      hsSrcIncludes = map ("-i" ++ ) hsSrcDirs
      -- See [Note: GHC now depends on exceptions package].
      cmd = unwords $
        [ "stack exec" ] ++
        [ "--package exceptions" | ghcFlavor >= Ghc901 ] ++
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
        [ "-package exceptions" | ghcFlavor >= Ghc901 ] ++
        hsSrcIncludes ++
        map (stage0Compiler </>)
             (if ghcFlavor >= Ghc901
                then ["GHC/Parser.hs"]
                else ["Parser.hs"]
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
        hsSrcDirs
      -- Lastly, manipulate text like 'GHC/Exts/Heap/Constants.hs'
      -- into 'GHC.Exts.Heap.Constants'.
      modules = map (replace "/" "." . dropSuffix ".hs") strippedModulePaths
      -- The modules in this list elude being listed in
      -- '.parser-depends' but are required by ghc-lib-parser. We
      -- intervene and patch things up.
      extraModules =
        [ x | ghcFlavor > Ghc921
            , x <- [ "GHC.Runtime.Interpreter"
                   , "GHCi.BinaryArray"
                   , "GHCi.BreakArray"
                   , "GHCi.ResolvedBCO"
                   , "GHC.Driver.Config.Parser"
                   ]
        ] ++
        [ x | ghcFlavor > Ghc901
            ,  x <- [ "GHC.Driver.Config"
                    , "GHC.Parser.Errors.Ppr"
                    ]
        ] ++
        [ if ghcFlavor >= Ghc901 then "GHC.Parser.Header" else "HeaderInfo"
        , if ghcFlavor >= Ghc8101 then "GHC.Hs.Dump" else "HsDumpAst"
        ]
  return $ nubSort (modules ++ extraModules)

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
  when (ghcFlavor <= Ghc921) $ do
    renameFileRewriteSrcs "compiler" "HsVersions.h" ["compiler", stage0Compiler] [".hs", ".y", ".hsc"]

-- Rename 'DerivedConstants.h' to 'GhclibDerivedConstants.h'.
applyPatchDerivedConstants :: GhcFlavor -> IO ()
applyPatchDerivedConstants ghcFlavor =
  renameFileRewriteSrcs (hadrianGeneratedRoot ghcFlavor) "DerivedConstants.h" ["compiler", stage0Compiler] [".hs", ".y", ".hsc"]

-- Selectively disable optimizations in some particular files so as
-- to reduce (user) compile times. The files we apply this to were
-- those identified as bottlenecks in the 2019 GSoC Hadrian speed
-- project.
applyPatchDisableCompileTimeOptimizations :: GhcFlavor -> IO ()
applyPatchDisableCompileTimeOptimizations ghcFlavor =
    let files =
          case ghcFlavor of
            GhcMaster -> [ "compiler/GHC/Driver/Session.hs", "compiler/GHC/Hs.hs" ]
            Ghc921 ->    [ "compiler/GHC/Driver/Session.hs", "compiler/GHC/Hs.hs" ]
            Ghc901 ->    [ "compiler/GHC/Driver/Session.hs", "compiler/GHC/Hs.hs" ]
            Ghc8101 ->   [ "compiler/main/DynFlags.hs", "compiler/GHC/Hs.hs" ]
            Ghc8102 ->   [ "compiler/main/DynFlags.hs", "compiler/GHC/Hs.hs" ]
            Ghc8103 ->   [ "compiler/main/DynFlags.hs", "compiler/GHC/Hs.hs" ]
            Ghc8104 ->   [ "compiler/main/DynFlags.hs", "compiler/GHC/Hs.hs" ]
            Ghc8105 ->   [ "compiler/main/DynFlags.hs", "compiler/GHC/Hs.hs" ]
            _ ->         [ "compiler/main/DynFlags.hs", "compiler/hsSyn/HsInstances.hs" ]
    in
      forM_ files $
        \file ->
          writeFile file .
          ("{-# OPTIONS_GHC -O0 #-}\n" ++)
          =<< readFile' file

applyPatchGHCiInfoTable :: GhcFlavor -> IO ()
applyPatchGHCiInfoTable ghcFlavor =
  when(ghcFlavor == GhcMaster) $ do
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
        "#error Sorry, rts versions <= 1.0 are not supported"
        (unlines
         [  "foreign import ccall unsafe \"allocateExec\""
          , "  _allocateExec :: CUInt -> Ptr (Ptr a) -> IO (Ptr a)"
          , ""
          , "foreign import ccall unsafe \"flushExec\""
          , "  _flushExec :: CUInt -> Ptr a -> IO ()" ])
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
  when (ghcFlavor >= Ghc921) $ do
    -- Synthesize a definition of MIN_VERSION_ghc_heap. If X.Y.Z is
    -- the current version, then MIN_VERSION_ghc_heap(a, b, c) is a
    -- test of whether X.Y.Z >= a.b.c (that is, ghc-heap X.Y.Z is at
    -- least a.b.c).
    versionText <- readFile' (hadrianGeneratedRoot ghcFlavor </> "ghcversion.h")
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

-- Users of ghc-lib-parser-9.0.* have reported GHC tripping up on a
-- comment in this particular source file (see
-- https://github.com/ndmitchell/hlint/issues/1224 for example). The
-- comment has been changed in the way we do here on HEAD. We patch it
-- here for flavor 9.0.
applyPatchHaddockHs :: GhcFlavor -> IO ()
applyPatchHaddockHs ghcFlavor = do
  when (ghcFlavor == Ghc901) (
    writeFile haddockHs .
      replace
        "-- *"
        "-- -"
    =<< readFile' haddockHs )
    where
      haddockHs = "compiler/GHC/Parser/PostProcess/Haddock.hs"

-- Support for unboxed tuples got landed 03/20/2021
-- (https://gitlab.haskell.org/ghc/ghc/-/commit/1f94e0f7601f8e22fdd81a47f130650265a44196#4ec156a7b95e9c7a690c99bc79e6e0edf60a51dc)
-- Older versions of the rts don't define two of the numeric
-- instruction codes that this support relies on.
applyPatchRtsBytecodes :: GhcFlavor -> IO ()
applyPatchRtsBytecodes ghcFlavor = do
  when (ghcFlavor >= Ghc921) (
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
          if ghcFlavor >= Ghc901
            then "GHC/Builtin/Types/Prim.hs"
            else "prelude/TysPrim.hs"
    when (ghcFlavor >= Ghc901) (
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
    when (ghcFlavor >= Ghc901) (
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
        [ "compiler/GHC/Runtime/Heap/Layout.hs" | ghcFlavor >= Ghc901 ] ++
        [ "compiler/cmm/SMRep.hs" | ghcFlavor < Ghc901 ] ++
        [ "compiler/GHC/StgToCmm/Layout.hs"  | ghcFlavor >= Ghc8101 ] ++
        [ "compiler/codeGen/StgCmmLayout.hs" | ghcFlavor < Ghc8101 ]
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
    let files =
          if ghcFlavor >= Ghc901
          then
            [ "compiler/GHC/Types/Unique/Supply.hs"
            , "compiler/GHC/Types/Unique.hs"
            ]
          else
            [ "compiler/basicTypes/UniqSupply.hs" ]
    forM_ files $ \file ->
        writeFile file .
        prefixForeignImport genSym .
        prefixForeignImport initGenSym
        =<< readFile' file
    let cUtils =
          if ghcFlavor >= Ghc901
            then
              [ "compiler/cbits/cutils.c" ]
            else
              [ "compiler/parser/cutils.c", "compiler/parser/cutils.h" ]
    forM_ cUtils $ \file ->
        writeFile file .
        prefixSymbol enableTimingStats .
        prefixSymbol setHeapSize
        =<< readFile' file
    let file =
          if ghcFlavor >= Ghc901
            then "compiler/GHC/Driver/Session.hs"
            else "compiler/main/DynFlags.hs"
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
  when (ghcFlavor < Ghc8101) $
    forM_ [ "compiler/ghci/Linker.hs"
          , "compiler/utils/FastString.hs"
          , "compiler/main/DynFlags.hs"
          ] $
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
  when (ghcFlavor <= Ghc901) $
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
    forM_ [ "libraries/ghc-heap/GHC/Exts/Heap/InfoTable.hsc"
          , "libraries/ghc-heap/GHC/Exts/Heap/InfoTableProf.hsc"
          ] $
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

-- [Note : GHC now depends on exceptions package]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- As of
-- https://gitlab.haskell.org/ghc/ghc/-/commit/30272412fa437ab8e7a8035db94a278e10513413
-- (4th May 2020), certain GHC modules depend on the exceptions
-- package. Depending on the boot compiler, this package may or may
-- not be present and if it's missing, determining the list of
-- ghc-lib-parser modules (via 'calcParserModules') will fail. The
-- function 'appyPatchHadrianStackYaml' guarantees it is available
-- if its needed.

-- | Patch Hadrian's Cabal.
applyPatchHadrianStackYaml :: GhcFlavor -> IO ()
applyPatchHadrianStackYaml ghcFlavor = do
  let hadrianStackYaml = "hadrian/stack.yaml"
  config <- Y.decodeFileThrow hadrianStackYaml
  -- See [Note : GHC now depends on exceptions package]
  let deps = ["exceptions-0.10.4" | ghcFlavor >= Ghc901 ] ++
        case parse (\cfg -> cfg .:? "extra-deps" .!= []) config of
          Success ls -> ls :: [Y.Value]
          Error msg -> error msg
 -- Build hadrian (and any artifacts we generate via hadrian e.g.
 -- Parser.hs) as quickly as possible.
  let opts = HMS.insert "$everything" "-O0 -j" $
        case parse (\cfg -> cfg .:? "ghc-options" .!= HMS.empty) config of
          Success os -> os :: HMS.HashMap T.Text Y.Value
          Error msg -> error msg
  let config' = HMS.insert "extra-deps" (toJSON deps) (HMS.insert "ghc-options" (toJSON opts) config)
  Y.encodeFile hadrianStackYaml config'

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
baseBounds ghcFlavor =
  case ghcFlavor of
    -- ghc >= 8.4.4
    DaGhc881  -> "base >= 4.11 && < 4.16"
    Ghc881    -> "base >= 4.11 && < 4.16"
    Ghc882    -> "base >= 4.11 && < 4.16"
    Ghc883    -> "base >= 4.11 && < 4.16"
    Ghc884    -> "base >= 4.11 && < 4.16"

    Ghc8101   -> "base >= 4.12 && < 4.16"
    Ghc8102   -> "base >= 4.12 && < 4.16"
    Ghc8103   -> "base >= 4.12 && < 4.16"
    Ghc8104   -> "base >= 4.12 && < 4.16"
    Ghc8105   -> "base >= 4.12 && < 4.16"

    Ghc901    -> "base >= 4.13 && < 4.16"
    Ghc921    -> "base >= 4.14 && < 4.17"

    GhcMaster -> "base >= 4.14 && < 4.17"

-- | Common build dependencies.
commonBuildDepends :: GhcFlavor -> [String]
commonBuildDepends ghcFlavor =
  [ "ghc-prim > 0.2 && < 0.8"
  , baseBounds ghcFlavor
  , "containers >= 0.5 && < 0.7"
  , "bytestring >= 0.9 && < 0.11"
  , "binary == 0.8.*"
  , "filepath >= 1 && < 1.5"
  , "directory >= 1 && < 1.4"
  , "array >= 0.1 && < 0.6"
  , "deepseq >= 1.4 && < 1.5"
  , "pretty == 1.1.*"
  , "time >= 1.4 && < 1.10"
  , "transformers == 0.5.*"
  , "process >= 1 && < 1.7"
  ] ++
  [ "exceptions == 0.10.*" | ghcFlavor >= Ghc901 ] ++
  [ "parsec" | ghcFlavor >= Ghc921 ]

ghcLibParserBuildDepends :: GhcFlavor -> [String]
ghcLibParserBuildDepends  = commonBuildDepends

ghcLibBuildDepends :: GhcFlavor -> [String]
ghcLibBuildDepends ghcFlavor =
  commonBuildDepends ghcFlavor ++
  [ "rts"
  , "hpc == 0.6.*"
  , "ghc-lib-parser"
  ]

-- | This utility factored out to avoid repetion.
libBinParserModules :: GhcFlavor -> IO ([Cabal], [Cabal], [String])
libBinParserModules ghcFlavor = do
    lib <- mapM readCabalFile cabalFileLibraries
    bin <- readCabalFile cabalFileBinary
    parserModules <- calcParserModules ghcFlavor
    return (lib, [bin], parserModules)

-- | Call this after cabal file generation. These files are generated
-- from '.hsc' files in the source tree and we prefer to ship those in
-- the sdists rather than these. If we don't remove them, some
-- ambiguity results. Cabal and stack are OK with that but bazel gets
-- messed up.
removeGeneratedIntermediateFiles :: GhcFlavor -> IO ()
removeGeneratedIntermediateFiles ghcFlavor = do
    removeFile $ stage0GhcHeap </> "GHC/Exts/Heap/Utils.hs"
    removeFile $ stage0GhcHeap </> "GHC/Exts/Heap/InfoTableProf.hs"
    removeFile $ stage0GhcHeap </> "GHC/Exts/Heap/InfoTable.hs"
    removeFile $ stage0GhcHeap </> "GHC/Exts/Heap/InfoTable/Types.hs"
    removeFile $ stage0GhcHeap </> "GHC/Exts/Heap/Constants.hs"
    removeFile $ stage0Ghci </> "GHCi/FFI.hs"
    when (ghcFlavor < Ghc921) $
      removeFile $ stage0Ghci </> "GHCi/InfoTable.hs"

-- | Produces a ghc-lib Cabal file.
generateGhcLibCabal :: GhcFlavor -> IO ()
generateGhcLibCabal ghcFlavor = do
    -- Compute the list of modules to be compiled. The rest are parser
    -- modules re-exported from ghc-lib-parser.
    (lib, _bin, parserModules) <- libBinParserModules ghcFlavor
    let nonParserModules =
          Set.toList (Set.difference
          (Set.fromList (askField lib "exposed-modules:" ))
          (Set.fromList parserModules))

    writeFile "ghc-lib.cabal" $ unlines $ map trimEnd $
        [ "cabal-version: >=1.22"
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
        , ""
        , "library"
        , "    default-language:   Haskell2010"
        , "    exposed: False"
        , "    include-dirs:"
        ] ++
        indent2 (ghcLibIncludeDirs ghcFlavor) ++
        [ "    ghc-options: -fobject-code -package=ghc-boot-th -optc-DTHREADED_RTS"
        , "    cc-options: -DTHREADED_RTS"
        , "    cpp-options: " <> ghcStageDef ghcFlavor <> " -DTHREADED_RTS " <> ghciDef ghcFlavor <> ghcInGhciDef ghcFlavor
        , "    if !os(windows)"
        , "        build-depends: unix"
        , "    else"
        , "        build-depends: Win32"
        , "    build-depends:" ] ++
        indent2 (withCommas (ghcLibBuildDepends ghcFlavor))++
        [ "    build-tools: alex >= 3.1, happy >= 1.19.4"
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
    removeGeneratedIntermediateFiles ghcFlavor
    putStrLn "# Generating 'ghc-lib.cabal'... Done!"

ghciDef :: GhcFlavor -> String
ghciDef ghcFlavor = if ghcFlavor > Ghc8101 then "" else "-DGHCI"

ghcStageDef :: GhcFlavor -> String
ghcStageDef ghcFlavor = if ghcFlavor >= Ghc8101 then "" else "-DSTAGE=2"

ghcInGhciDef :: GhcFlavor -> String
ghcInGhciDef ghcFlavor = if ghcFlavor > Ghc901 then "" else " -DGHC_IN_GHCI "

-- Perform a set of specific substitutions on the given list of files.
performExtraFilesSubstitutions :: GhcFlavor -> (GhcFlavor -> [FilePath]) -> [FilePath]
performExtraFilesSubstitutions ghcFlavor files =
  foldl' sub (files ghcFlavor) $
      [ (hadrianGeneratedRoot ghcFlavor </> "ghcversion.h", Nothing)
      , (hadrianGeneratedRoot ghcFlavor </> "DerivedConstants.h", Just $ hadrianGeneratedRoot ghcFlavor </> "GhclibDerivedConstants.h")
      ] ++
      [("compiler" </> "HsVersions.h", Just $ "compiler" </> "GhclibHsVersions.h")
      | ghcFlavor <= Ghc921
      ]
  where
    sub :: Eq a => [a] -> (a, Maybe a) -> [a]
    sub xs (s, r) = replace [s] (maybeToList r) xs

-- | Produces a ghc-lib-parser Cabal file.
generateGhcLibParserCabal :: GhcFlavor -> IO ()
generateGhcLibParserCabal ghcFlavor = do
    (lib, _bin, parserModules) <- libBinParserModules ghcFlavor
    writeFile "ghc-lib-parser.cabal" $ unlines $ map trimEnd $
        [ "cabal-version: >=1.22"
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
        , ""
        , "library"
        , "    default-language:   Haskell2010"
        , "    exposed: False"
        , "    include-dirs:"] ++ indent2 (ghcLibParserIncludeDirs ghcFlavor) ++
        [ "    ghc-options: -fobject-code -package=ghc-boot-th -optc-DTHREADED_RTS"
        , "    cc-options: -DTHREADED_RTS"
        , "    cpp-options: " <> ghcStageDef ghcFlavor <> " -DTHREADED_RTS " <> ghciDef ghcFlavor <> ghcInGhciDef ghcFlavor
        , "    if !os(windows)"
        , "        build-depends: unix"
        , "    else"
        , "        build-depends: Win32"
        , "    build-depends:" ] ++
        indent2 (withCommas (ghcLibParserBuildDepends ghcFlavor)) ++
        [ "    build-tools: alex >= 3.1, happy >= 1.19.4"
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
        indent2 [ if ghcFlavor >= Ghc901 then "compiler/cbits/cutils.c" else "compiler/parser/cutils.c" ] ++
        [ "    hs-source-dirs:" ] ++
        indent2 (ghcLibParserHsSrcDirs False ghcFlavor lib) ++
        [ "    autogen-modules:" ] ++
        (if  ghcFlavor >= Ghc901
          then
           [ "        GHC.Parser.Lexer"
           , "        GHC.Parser"
           ]
          else
           [ "        Lexer"
           , "        Parser"
           ]
        ) ++
        ["    exposed-modules:" ] ++ indent2 parserModules
    removeGeneratedIntermediateFiles ghcFlavor
    putStrLn "# Generating 'ghc-lib-parser.cabal'... Done!"

-- | Run Hadrian to build the things that the Cabal files need.
generatePrerequisites :: GhcFlavor -> IO ()
generatePrerequisites ghcFlavor = do
  when (ghcFlavor < Ghc8101) (
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
    system_ $ unwords $
        [ "stack exec hadrian --"
          , "--directory=.."
          , "--build-root=ghc-lib"
        ] ++
        (if ghcFlavor >= Ghc901 then ["--bignum=native"] else ["--integer-simple"]) ++
        ghcLibParserExtraFiles ghcFlavor ++ map (dataDir </>) (dataFiles ghcFlavor)

  -- We use the hadrian generated Lexer and Parser so get these out
  -- of the way.
  let lexer = if ghcFlavor >= Ghc901 then "compiler/GHC/Parser/Lexer.x" else "compiler/parser/Lexer.x"
  let parser = if ghcFlavor >= Ghc901 then "compiler/GHC/Parser.y" else "compiler/parser/Parser.y"
  removeFile lexer
  removeFile parser
  when (ghcFlavor < Ghc8101) $
    removeFile "compiler/utils/Fingerprint.hsc" -- Favor the generated .hs file here too.
