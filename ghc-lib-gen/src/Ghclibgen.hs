-- Copyright (c) 2019, Digital Asset (Switzerland) GmbH and/or its
-- affiliates. All rights reserved.  SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

module Ghclibgen (
    applyPatchHeapClosures
  , applyPatchGhcPrim
  , applyPatchDisableCompileTimeOptimizations
  , applyPatchRtsIncludePaths
  , applyPatchStage
  , generatePrerequisites
  , mangleCSymbols
  , generateGhcLibCabal
  , generateGhcLibParserCabal
) where

import Control.Monad
import System.Process.Extra
import System.FilePath hiding ((</>))
import System.FilePath.Posix((</>)) -- Make sure we generate / on all platforms.
import System.Directory
import System.IO.Extra
import Data.List.Extra
import Data.Char
import Data.Maybe
import Data.Ord
import qualified Data.Set as Set

import GhclibgenOpts

-- Constants.

-- | Cabal files from libraries inside GHC that are merged together
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
  [ "includes" ] ++ -- ghcconfig.h, MachDeps.h, MachRegs.h, CodeGen.Platform.hs
  (case ghcFlavor of
    GhcMaster ->   ["ghc-lib/stage0/lib"]
    _ -> ["ghc-lib/generated"]) ++
  [ "ghc-lib/stage0/compiler/build"
  , "compiler"
  ] ++
  ["compiler/utils" | ghcFlavor /= GhcMaster]

-- Sort by length so the longest paths are at the front. We do this
-- so that in 'calcParserModules', longer substituions are performed
-- before shorter ones (and bad things will happen if that were
-- not the case).
sortDiffListByLength :: Set.Set FilePath -> Set.Set FilePath -> [FilePath]
sortDiffListByLength all excludes =
  sortBy (flip (comparing length)) $ Set.toList (Set.difference all excludes)

-- | The "hs-source-dirs" for 'ghc-lib-parser' (actually used in two
-- contexts, 'ghc-lib-parser.cabal' generation and when calculating
-- the set of parser modules).
ghcLibParserHsSrcDirs :: Bool -> GhcFlavor -> [Cabal] -> [FilePath]
ghcLibParserHsSrcDirs forParserDepends ghcFlavor lib =
  let all = Set.fromList $
        [ "ghc-lib/stage0/compiler/build"]
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
        ++ [dir | forParserDepends
                , dir <- ["ghc-lib/stage0/libraries/ghci/build"
                         ,"ghc-lib/stage0/libraries/ghc-heap/build"]
                ]
        -- This next conditional just smooths over a 'master'
        -- vs. '8.8.1' branch difference (relating to a file
        -- 'GhcVersion.hs' IIRC).
        ++ [ "ghc-lib/stage0/libraries/ghc-boot/build" | ghcFlavor == GhcMaster ]
        ++ map takeDirectory cabalFileLibraries
        ++ askFiles lib "hs-source-dirs:"

      excludes = Set.fromList $
        [ "compiler/codeGen"
        , "compiler/hieFile"
        , "compiler/llvmGen"
        , "compiler/rename"
        , "compiler/stgSyn"
        , "compiler/stranal"
        , "compiler/nativeGen"
        ] ++
        [ "compiler/deSugar" | ghcFlavor == GhcMaster && not forParserDepends]
  in sortDiffListByLength all excludes -- Very important. See the comment on 'sortDiffListByLength' above.

-- | C-preprocessor "include dirs" for 'ghc-lib'.
ghcLibIncludeDirs :: GhcFlavor -> [FilePath]
ghcLibIncludeDirs = ghcLibParserIncludeDirs -- Needs checking (good enough for now).

-- | The "hs-source-dirs" for 'ghc-lib'.
ghcLibHsSrcDirs :: GhcFlavor -> [Cabal] -> [FilePath]
ghcLibHsSrcDirs ghcFlavor lib =
  let all = Set.fromList $
        [ "ghc-lib/stage0/compiler/build"]
        ++ [ "ghc-lib/stage0/libraries/ghc-boot/build" | ghcFlavor == GhcMaster ] -- 'GHC.Platform' is in 'ghc-lib-parser', 'GHC.Platform.Host' is not.
        ++ map takeDirectory cabalFileLibraries
        ++ askFiles lib "hs-source-dirs:"
      excludes = Set.fromList
        [ "compiler/basicTypes"
        , "compiler/parser"
        , "compiler/types"
        , "libraries/ghc-boot-th"
        , "libraries/ghc-heap"
        ]
  in sortDiffListByLength all excludes -- Not so important. Here for symmetry with 'ghcLibParserHsSrcDirs' I think.

-- | Cabal file for the GHC binary.
cabalFileBinary :: FilePath
cabalFileBinary = "ghc/ghc-bin.cabal"

-- We set the hadrian build root to 'ghc-lib'.
stage0LibPath :: FilePath
stage0LibPath = "ghc-lib/stage0/lib"

-- |'dataDir' is the directory cabal looks for data files to install,
-- relative to the source directory.
dataDir :: FilePath
dataDir = stage0LibPath

-- |'dataFiles' is a list of files to be installed for run-time use by
-- the package.
dataFiles :: [FilePath]
dataFiles =
    [ "settings"
    , "llvm-targets"
    , "llvm-passes"
    , "platformConstants"
    ]

-- | See 'hadrian/src/Rules/Generate.hs'.

includesDependencies :: GhcFlavor -> [FilePath]
includesDependencies ghcFlavor =
   map (root ++ )
   [ "ghcautoconf.h"
   , "ghcplatform.h"
   , "ghcversion.h"]
   where
      root = (case ghcFlavor of
                GhcMaster -> stage0LibPath
                _ -> "ghc-lib/generated") ++ "/"

derivedConstantsDependencies :: GhcFlavor -> [FilePath]
derivedConstantsDependencies ghcFlavor =
   map (root ++)
   [ "DerivedConstants.h"
   , "GHCConstantsHaskellExports.hs"
   , "GHCConstantsHaskellType.hs"
   , "GHCConstantsHaskellWrappers.hs"
   ]
   where
      root = (case ghcFlavor of
                GhcMaster -> stage0LibPath
                _ -> "ghc-lib/generated") ++ "/"

compilerDependencies :: GhcFlavor -> [FilePath]
compilerDependencies _ =
    [ "ghc-lib/stage0/compiler/build/primop-can-fail.hs-incl"
    , "ghc-lib/stage0/compiler/build/primop-code-size.hs-incl"
    , "ghc-lib/stage0/compiler/build/primop-commutable.hs-incl"
    , "ghc-lib/stage0/compiler/build/primop-data-decl.hs-incl"
    , "ghc-lib/stage0/compiler/build/primop-fixity.hs-incl"
    , "ghc-lib/stage0/compiler/build/primop-has-side-effects.hs-incl"
    , "ghc-lib/stage0/compiler/build/primop-list.hs-incl"
    , "ghc-lib/stage0/compiler/build/primop-out-of-line.hs-incl"
    , "ghc-lib/stage0/compiler/build/primop-primop-info.hs-incl"
    , "ghc-lib/stage0/compiler/build/primop-strictness.hs-incl"
    , "ghc-lib/stage0/compiler/build/primop-tag.hs-incl"
    , "ghc-lib/stage0/compiler/build/primop-vector-tycons.hs-incl"
    , "ghc-lib/stage0/compiler/build/primop-vector-tys-exports.hs-incl"
    , "ghc-lib/stage0/compiler/build/primop-vector-tys.hs-incl"
    , "ghc-lib/stage0/compiler/build/primop-vector-uniques.hs-incl"
    ]

platformH :: GhcFlavor -> [FilePath]
platformH ghcFlavor = ["ghc-lib/stage0/compiler/build/ghc_boot_platform.h" | ghcFlavor /= GhcMaster]

packageCode :: GhcFlavor -> [FilePath]
packageCode ghcFlavor =
  "ghc-lib/stage0/compiler/build/Config.hs" :
  [ "ghc-lib/stage0/libraries/ghc-boot/build/GHC/Version.hs" | ghcFlavor == GhcMaster ]

fingerprint :: GhcFlavor -> [FilePath]
fingerprint ghcFlavor = ["ghc-lib/stage0/compiler/build/Fingerprint.hs" | ghcFlavor /= GhcMaster]

-- | The C headers shipped with ghc-lib. These globs get glommed onto
-- the 'extraFiles' above as 'extra-source-files'.
cHeaders :: GhcFlavor -> [String]
cHeaders ghcFlavor =
  [ "includes/ghcconfig.h"
  , "includes/MachDeps.h"
  , "includes/stg/MachRegs.h"
  , "includes/CodeGen.Platform.hs"
  , "compiler/HsVersions.h"
  , "compiler/Unique.h"
  ] ++
  [ f | ghcFlavor /= GhcMaster
    , f <- [ "compiler/nativeGen/NCG.h", "compiler/utils/md5.h"]
  ]

-- | We generate the parser and lexer and ship those rather than their
-- sources.
generatedParser :: GhcFlavor -> [FilePath]
generatedParser _ =
    [ "ghc-lib/stage0/compiler/build/Parser.hs"
    , "ghc-lib/stage0/compiler/build/Lexer.hs"]

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
      cmd = unwords $
        [ "stack exec --stack-yaml hadrian/stack.yaml -- ghc"
        , "-dep-suffix ''"
        , "-dep-makefile .parser-depends"
        , "-M"]
        ++ includeDirs
        ++ [ "-ignore-package ghc"
           , "-ignore-package ghci"
           , "-package base"
           ]
        ++ hsSrcIncludes
        ++ ["ghc-lib/stage0/compiler/build/Parser.hs"]
  putStrLn "# Generating 'ghc/.parser-depends'..."
  putStrLn $ "\n\n# Running: " ++ cmd
  system_ cmd

  buf <- readFile' ".parser-depends"
  -- The idea here is harvest from lines like
  -- 'compiler/prelude/PrelRules.o : compiler/prelude/PrelRules.hs',
  -- just the module name e.g. in this example, 'PrelRules'.
      -- Stip comment lines.
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
  -- We put 'HeaderInfo' here because doing so means clients who
  -- just want to do parsing don't need 'ghc-lib' (this module
  -- is needed for parsing in the presence of dynamic pragmas).
  return $ nubSort (modules ++ ["HeaderInfo"])

-- | Selectively disable optimizations in some particular files so as
-- to reduce (user) compile times. The files we apply this to were
-- those identified as bottlenecks in the 2019 GSoC Hadrian speed
-- project.
applyPatchDisableCompileTimeOptimizations :: GhcFlavor -> IO ()
applyPatchDisableCompileTimeOptimizations ghcFlavor =
    let files =
          "compiler/main/DynFlags.hs" :
          (case ghcFlavor of
            GhcMaster -> [ "compiler/GHC/Hs.hs" ]
            _ ->         [ "compiler/hsSyn/HsInstances.hs" ])
    in
      forM_ files $
        \file ->
          writeFile file .
          ("{-# OPTIONS_GHC -O0 #-}\n" ++)
          =<< readFile' file

-- | Stub out a couple of definitions in the ghc-heap library that
-- require CMM features, since Cabal doesn't work well with CMM files.
applyPatchHeapClosures :: GhcFlavor -> IO ()
applyPatchHeapClosures _ = do
    let file = "libraries/ghc-heap/GHC/Exts/Heap/Closures.hs"
    writeFile file .
        replace
            "foreign import prim \"aToWordzh\" aToWord# :: Any -> Word#"
            "aToWord# :: Any -> Word#\naToWord# _ = 0##\n" .
        replace
            "foreign import prim \"reallyUnsafePtrEqualityUpToTag\"\n    reallyUnsafePtrEqualityUpToTag# :: Any -> Any -> Int#"
            "reallyUnsafePtrEqualityUpToTag# :: Any -> Any -> Int#\nreallyUnsafePtrEqualityUpToTag# _ _ = 0#\n"
        =<< readFile' file

-- Workaround lack of newer ghc-prim 12/3/2019
-- (https://gitlab.haskell.org/ghc/ghc/commit/705a16df02411ec2445c9a254396a93cabe559ef)
applyPatchGhcPrim :: GhcFlavor -> IO ()
applyPatchGhcPrim ghcFlavor = do
    let tysPrim = "compiler/prelude/TysPrim.hs"
    when (ghcFlavor == GhcMaster) (
      writeFile tysPrim .
          replace
              "bcoPrimTyCon = pcPrimTyCon0 bcoPrimTyConName LiftedRep"
              "bcoPrimTyCon = pcPrimTyCon0 bcoPrimTyConName UnliftedRep" .
          replace
              "bcoPrimTyConName              = mkPrimTc (fsLit \"BCO\") bcoPrimTyConKey bcoPrimTyCon"
              "bcoPrimTyConName              = mkPrimTc (fsLit \"BCO#\") bcoPrimTyConKey bcoPrimTyCon"
        =<< readFile' tysPrim
        )
    let createBCO = "libraries/ghci/GHCi/CreateBCO.hs"
    when (ghcFlavor == GhcMaster) (
      writeFile createBCO .
          replace
              "do linked_bco <- linkBCO' arr bco"
              "do BCO bco# <- linkBCO' arr bco" .
          replace
              "then return (HValue (unsafeCoerce linked_bco))\n           else case mkApUpd0# linked_bco of { (# final_bco #) ->"
              "then return (HValue (unsafeCoerce# bco#))\n           else case mkApUpd0# bco# of { (# final_bco #) ->" .
          replace
              "bco <- linkBCO' arr bco\n      writePtrsArrayBCO i bco marr"
              "BCO bco# <- linkBCO' arr bco\n      writePtrsArrayBCO i bco# marr" .
          replace
              "writePtrsArrayBCO :: Int -> BCO -> PtrsArr -> IO ()"
              "writePtrsArrayBCO :: Int -> BCO# -> PtrsArr -> IO ()" .
          replace
              "writePtrsArrayMBA :: Int -> MutableByteArray# s -> PtrsArr -> IO ()"
              "data BCO = BCO BCO#\nwritePtrsArrayMBA :: Int -> MutableByteArray# s -> PtrsArr -> IO ()" .
          replace
              "newBCO# instrs lits ptrs arity bitmap s"
              "case newBCO# instrs lits ptrs arity bitmap s of\n    (# s1, bco #) -> (# s1, BCO bco #)"
        =<< readFile' createBCO
        )

-- | Fix up these rts include paths. We don't ship rts headers since
-- we run ghc-lib using the RTS of the compiler we build with - we go
-- to the compiler installation for those.
applyPatchRtsIncludePaths :: GhcFlavor -> IO ()
applyPatchRtsIncludePaths flavor = do
  let files =
        [ "compiler/cmm/SMRep.hs"] ++
        ["compiler/GHC/StgToCmm/Layout.hs"  | flavor == GhcMaster] ++
        ["compiler/codeGen/StgCmmLayout.hs" | flavor /= GhcMaster]
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
mangleCSymbols _ = do
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
    let file = "compiler/basicTypes/UniqSupply.hs" in writeFile file .
        prefixForeignImport genSym .
        prefixForeignImport initGenSym
        =<< readFile' file
    forM_ ["compiler/parser/cutils.c", "compiler/parser/cutils.h"] $ \file ->
        writeFile file .
        prefixSymbol enableTimingStats .
        prefixSymbol setHeapSize
        =<< readFile' file
    let file = "compiler/main/DynFlags.hs" in
        writeFile file .
        prefixForeignImport enableTimingStats .
        prefixForeignImport setHeapSize
        =<< readFile' file

-- Setting DSTAGE=2 will cause GHC to use getOrSetLibHSghc in FastString,
-- DynFlags and Linker so we patch away that usage while leaving -DSTAGE=2 on
-- since it is useful in other places, e.g., MachDeps.h.
--
-- See https://github.com/ndmitchell/hlint/issues/637 for an issue caused
-- by using getOrSetLibHSghc for the FastString table.
applyPatchStage :: GhcFlavor -> IO ()
applyPatchStage ghcFlavor =
  -- On master, `ghcplatform.h` sets `GHC_STAGE` to `1` and we no
  -- longer are required to pass `-DGHC_STAGE=2` to `cpp-options` to
  -- get a build (`MachDeps.h` does not hide its contents from stages
  -- below 2 anymore). All usages of `getOrSetLibHSghc*` require
  -- `GHC_STAGE >= 2` . Thus, it's no longer neccessary to patch here.
  when (ghcFlavor /= GhcMaster) $
    forM_ ["compiler/ghci/Linker.hs"
          , "compiler/utils/FastString.hs"
          , "compiler/main/DynFlags.hs"] $
    \file ->
      (if ghcFlavor == GhcMaster
        then
          writeFile file . replace "GHC_STAGE >= 2" "0" . replace "GHC_STAGE < 2" "1"
        else
          writeFile file . replace "STAGE >= 2" "0" . replace "STAGE < 2" "1"
      )
      =<< readFile' file

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
        f (x:xs) = let (a,b) = break (":" `isSuffixOf`) xs in ((lower x,a),b)

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
indent = map ("    "++)
indent2 :: [String] -> [String]
indent2 = indent . indent
withCommas :: [String] -> [String]
withCommas ms =
  let ms' = reverse ms in
    reverse (head ms' : map (++",") (tail ms'))

-- | Common build dependencies.
commonBuildDepends :: [String]
commonBuildDepends =
  [ "ghc-prim > 0.2 && < 0.6"
  , "base >= 4.11 && < 4.14"
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
  , "hpc == 0.6.*"
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
removeGeneratedIntermediateFiles :: IO ()
removeGeneratedIntermediateFiles = do
    removeFile "ghc-lib/stage0/libraries/ghc-heap/build/GHC/Exts/Heap/Utils.hs"
    removeFile "ghc-lib/stage0/libraries/ghc-heap/build/GHC/Exts/Heap/InfoTableProf.hs"
    removeFile "ghc-lib/stage0/libraries/ghc-heap/build/GHC/Exts/Heap/InfoTable.hs"
    removeFile "ghc-lib/stage0/libraries/ghc-heap/build/GHC/Exts/Heap/InfoTable/Types.hs"
    removeFile "ghc-lib/stage0/libraries/ghc-heap/build/GHC/Exts/Heap/Constants.hs"
    removeFile "ghc-lib/stage0/libraries/ghci/build/GHCi/FFI.hs"
    removeFile "ghc-lib/stage0/libraries/ghci/build/GHCi/InfoTable.hs"

-- | Produces a ghc-lib Cabal file.
generateGhcLibCabal :: GhcFlavor -> IO ()
generateGhcLibCabal ghcFlavor = do
    -- Compute the list of modules to be compiled. The rest are parser
    -- modules re-exported from ghc-lib-parser.
    (lib, bin, parserModules) <- libBinParserModules ghcFlavor
    let nonParserModules =
          Set.toList (Set.difference
          (Set.fromList (askField lib "exposed-modules:" ))
          (Set.fromList parserModules))

    writeFile "ghc-lib.cabal" $ unlines $ map trimEnd $
        ["cabal-version: >=1.22"
        ,"build-type: Simple"
        ,"name: ghc-lib"
        ,"version: 0.1.0"
        ,"license: BSD3"
        ,"license-file: LICENSE"
        ,"category: Development"
        ,"author: The GHC Team and Digital Asset"
        ,"maintainer: Digital Asset"
        ,"synopsis: The GHC API, decoupled from GHC versions"
        ,"description: A package equivalent to the @ghc@ package, but which can be loaded on many compiler versions."
        ,"homepage: https://github.com/digital-asset/ghc-lib"
        ,"bug-reports: https://github.com/digital-asset/ghc-lib/issues"
        -- This is a nice idea but requires a very modern cabal which
        -- hinders uploading to hackage.
        -- ,"exposed: False" -- automatically hide `ghc-lib` (thanks Ed Kmett!)
        ,"data-dir: " ++ dataDir
        ,"data-files:"] ++ indent dataFiles ++
        ["extra-source-files:"] ++ indent (ghcLibExtraFiles ghcFlavor) ++
        ["tested-with: GHC==8.8.1, GHC==8.6.5, GHC==8.4.3"
        ,"source-repository head"
        ,"    type: git"
        ,"    location: git@github.com:digital-asset/ghc-lib.git"
        ,""
        ,"library"
        ,"    default-language:   Haskell2010"
        ,"    default-extensions: NoImplicitPrelude"
        ,"    exposed: False"
        ,"    include-dirs:"] ++
        indent2 (ghcLibIncludeDirs ghcFlavor) ++
        ["    ghc-options: -fobject-code -package=ghc-boot-th -optc-DTHREADED_RTS"
        ,"    cc-options: -DTHREADED_RTS"
        ,"    cpp-options: " <> ghcStageDef ghcFlavor <> " -DTHREADED_RTS " <> ghciDef ghcFlavor <> " -DGHC_IN_GHCI"
        ,"    if !os(windows)"
        ,"        build-depends: unix"
        ,"    else"
        ,"        build-depends: Win32"
        ,"    build-depends:"]++
        indent2 (withCommas (commonBuildDepends ++ ["ghc-lib-parser"]))++
        ["    build-tools: alex >= 3.1, happy >= 1.19.4"
        ,"    other-extensions:"] ++
        indent2 (askField lib "other-extensions:") ++
        ["    hs-source-dirs:"] ++
        indent2 (ghcLibHsSrcDirs ghcFlavor lib) ++
        ["    autogen-modules:"
        ,"        Paths_ghc_lib"
        ] ++
        ["    reexported-modules:"]++
        withCommas (indent2 $ nubSort parserModules) ++
        ["    exposed-modules:"
        ,"        Paths_ghc_lib"
        ] ++
        indent2 (nubSort nonParserModules)
    removeGeneratedIntermediateFiles
    putStrLn "# Generating 'ghc-lib.cabal'... Done!"

ghciDef :: GhcFlavor -> String
ghciDef GhcMaster = ""
ghciDef _ = "-DGHCI"

ghcStageDef :: GhcFlavor -> String
ghcStageDef GhcMaster = ""
ghcStageDef _ = "-DSTAGE=2"

-- | Produces a ghc-lib-parser Cabal file.
generateGhcLibParserCabal :: GhcFlavor -> IO ()
generateGhcLibParserCabal ghcFlavor = do
    (lib, bin, parserModules) <- libBinParserModules ghcFlavor
    writeFile "ghc-lib-parser.cabal" $ unlines $ map trimEnd $
        ["cabal-version: >=1.22"
        ,"build-type: Simple"
        ,"name: ghc-lib-parser"
        ,"version: 0.1.0"
        ,"license: BSD3"
        ,"license-file: LICENSE"
        ,"category: Development"
        ,"author: The GHC Team and Digital Asset"
        ,"maintainer: Digital Asset"
        ,"synopsis: The GHC API, decoupled from GHC versions"
        ,"description: A package equivalent to the @ghc@ package, but which can be loaded on many compiler versions."
        ,"homepage: https://github.com/digital-asset/ghc-lib"
        ,"bug-reports: https://github.com/digital-asset/ghc-lib/issues"
        -- This is a nice idea but requires a very modern cabal which
        -- hinders uploading to hackage.
        -- ,"exposed: False" -- automatically hide `ghc-lib` (thanks Ed Kmett!)
        ,"data-dir: " ++ dataDir
        ,"data-files:"] ++ indent dataFiles ++
        ["extra-source-files:"] ++ indent (ghcLibParserExtraFiles ghcFlavor) ++
        ["tested-with: GHC==8.8.1, GHC==8.6.5, GHC==8.4.3"
        ,"source-repository head"
        ,"    type: git"
        ,"    location: git@github.com:digital-asset/ghc-lib.git"
        ,""
        ,"library"
        ,"    default-language:   Haskell2010"
        ,"    default-extensions: NoImplicitPrelude"
        ,"    exposed: False"
        ,"    include-dirs:"] ++ indent2 (ghcLibParserIncludeDirs ghcFlavor) ++
        ["    ghc-options: -fobject-code -package=ghc-boot-th -optc-DTHREADED_RTS"
        ,"    cc-options: -DTHREADED_RTS"
        ,"    cpp-options: " <> ghcStageDef ghcFlavor <> " -DTHREADED_RTS " <> ghciDef ghcFlavor <> " -DGHC_IN_GHCI"
        ,"    if !os(windows)"
        ,"        build-depends: unix"
        ,"    else"
        ,"        build-depends: Win32"
        ,"    build-depends:"] ++ indent2 (withCommas commonBuildDepends) ++
        ["    build-tools: alex >= 3.1, happy >= 1.19.4"
        ,"    other-extensions:"] ++ indent2 (askField lib "other-extensions:") ++
        ["    c-sources:"] ++
        -- We hardcode these because the inclusion of 'keepCAFsForGHCi'
        -- causes issues in ghci see
        -- https://github.com/digital-asset/ghc-lib/issues/27
        indent2 ["compiler/cbits/genSym.c","compiler/parser/cutils.c"] ++
        ["    hs-source-dirs:"] ++
        indent2 (ghcLibParserHsSrcDirs False ghcFlavor lib) ++
        ["    autogen-modules:"
        ,"        Lexer"
        ,"        Parser"
        ] ++
        ["    exposed-modules:"] ++ indent2 parserModules
    removeGeneratedIntermediateFiles
    putStrLn "# Generating 'ghc-lib-parser.cabal'... Done!"

-- | Run Hadrian to build the things that the Cabal files need.
generatePrerequisites :: GhcFlavor -> IO ()
generatePrerequisites ghcFlavor = do
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
        ["stack exec hadrian --"
        ,"--directory=.."
        ,"--integer-simple"
        ,"--build-root=ghc-lib"
        ] ++ ghcLibParserExtraFiles ghcFlavor ++
        map (dataDir </>) dataFiles
  -- We use the hadrian generated Lexer and Parser so get these out
  -- of the way.
  removeFile "compiler/parser/Lexer.x"
  removeFile "compiler/parser/Parser.y"
  when (ghcFlavor /= GhcMaster) $
    removeFile "compiler/utils/Fingerprint.hsc" -- Favor the generated .hs file here too.
