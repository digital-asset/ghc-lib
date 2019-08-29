-- Copyright (c) 2019, Digital Asset (Switzerland) GmbH and/or its
-- affiliates. All rights reserved.  SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

module Ghclibgen (
    applyPatchHeapClosures
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
ghcLibParserIncludeDirs :: [FilePath]
ghcLibParserIncludeDirs =
  [ "includes" -- ghcconfig.h, MachDeps.h, CodeGen.Platform.hs
  , "ghc-lib/generated"
  , "ghc-lib/stage0/compiler/build"
  , "ghc-lib/stage1/compiler/build"
  , "compiler"
  , "compiler/utils"
  ]

-- Sort by length so the longest paths are at the front. We do this
-- so that in 'calcParserModules', longer substituions are performed
-- before shorter ones (and bad things will happen if that were
-- not the case).
sortDiffListByLength :: Set.Set FilePath -> Set.Set FilePath -> [FilePath]
sortDiffListByLength all excludes =
  sortBy (flip (comparing length)) $ Set.toList (Set.difference all excludes)

-- | The "hs-source-dirs" for 'ghc-lib-parser'.
ghcLibParserHsSrcDirs :: [Cabal] -> [FilePath]
ghcLibParserHsSrcDirs lib =
  let all = Set.fromList $
        [ "ghc-lib/stage0/compiler/build"
        , "ghc-lib/stage1/compiler/build"
        , "ghc-lib/stage0/libraries/ghci/build"
        , "ghc-lib/stage0/libraries/ghc-heap/build"
        ]
        ++ map takeDirectory cabalFileLibraries
        ++ askFiles lib "hs-source-dirs:"
      excludes = Set.fromList
        [ "compiler/codeGen"
        , "compiler/hieFile"
        , "compiler/llvmGen"
        , "compiler/rename"
        , "compiler/stgSyn"
        , "compiler/stranal"
        ]
  in sortDiffListByLength all excludes

-- | The "hs-source-dirs" for 'ghc-lib'.
ghcLibHsSrcDirs :: [Cabal] -> [FilePath]
ghcLibHsSrcDirs lib =
  let all = Set.fromList $
        [ "ghc-lib/stage1/compiler/build"
        -- , "ghc-lib/stage0/libraries/ghci/build"
        -- No, don't include this. Generate 'InfoTable.hs'
        -- via the libraries/ghci, .hsc file. The hazel/bazel build
        -- breaks when we allow this source dir.
        ]
        ++ map takeDirectory cabalFileLibraries
        ++ askFiles lib "hs-source-dirs:"
      excludes = Set.fromList
        [ "compiler/basicTypes"
        , "compiler/parser"
        , "compiler/types"
        , "libraries/ghc-boot-th"
        , "libraries/ghc-heap"
        ]
  in sortDiffListByLength all excludes

-- | C-preprocessor "include dirs" for 'ghc-lib'.
ghcLibIncludeDirs :: [FilePath]
ghcLibIncludeDirs = ghcLibParserIncludeDirs -- Needs adjusting.

-- | Cabal file for the GHC binary.
cabalFileBinary :: FilePath
cabalFileBinary = "ghc/ghc-bin.cabal"

-- |'dataDir' is the directory cabal looks for data files to install,
-- relative to the source directory.
dataDir :: FilePath
dataDir = "ghc-lib/stage1/lib"

-- |'dataFiles' is a list of files to be installed for run-time use by
-- the package.
dataFiles :: [FilePath]
dataFiles =
    [ "settings"
    , "llvm-targets"
    , "llvm-passes"
    , "platformConstants"
    ]

-- | Additional source and data files for Cabal. The files in this
-- list are all created by Hadrian.
extraFiles :: [FilePath]
extraFiles =
    -- See ghc/hadrian/src/Rules/Generate.hs
    [ "ghc-lib/generated/ghcautoconf.h"
    , "ghc-lib/generated/ghcplatform.h"
    , "ghc-lib/generated/ghcversion.h"
    , "ghc-lib/generated/DerivedConstants.h"
    , "ghc-lib/generated/GHCConstantsHaskellExports.hs"
    , "ghc-lib/generated/GHCConstantsHaskellType.hs"
    , "ghc-lib/generated/GHCConstantsHaskellWrappers.hs"
    , "ghc-lib/stage1/compiler/build/ghc_boot_platform.h"
    , "ghc-lib/stage1/compiler/build/primop-can-fail.hs-incl"
    , "ghc-lib/stage1/compiler/build/primop-code-size.hs-incl"
    , "ghc-lib/stage1/compiler/build/primop-commutable.hs-incl"
    , "ghc-lib/stage1/compiler/build/primop-data-decl.hs-incl"
    , "ghc-lib/stage1/compiler/build/primop-fixity.hs-incl"
    , "ghc-lib/stage1/compiler/build/primop-has-side-effects.hs-incl"
    , "ghc-lib/stage1/compiler/build/primop-list.hs-incl"
    , "ghc-lib/stage1/compiler/build/primop-out-of-line.hs-incl"
    , "ghc-lib/stage1/compiler/build/primop-primop-info.hs-incl"
    , "ghc-lib/stage1/compiler/build/primop-strictness.hs-incl"
    , "ghc-lib/stage1/compiler/build/primop-tag.hs-incl"
    , "ghc-lib/stage1/compiler/build/primop-vector-tycons.hs-incl"
    , "ghc-lib/stage1/compiler/build/primop-vector-tys-exports.hs-incl"
    , "ghc-lib/stage1/compiler/build/primop-vector-tys.hs-incl"
    , "ghc-lib/stage1/compiler/build/primop-vector-uniques.hs-incl"
    , "ghc-lib/stage0/compiler/build/Fingerprint.hs"
    -- Be careful not to add these files to a ghc-lib.cabal, just
    -- ghc-lib-parser.cabal.
    , "ghc-lib/stage1/compiler/build/Config.hs"
    , "ghc-lib/stage0/compiler/build/Parser.hs"
    , "ghc-lib/stage0/compiler/build/Lexer.hs"
    ]

-- | The C headers shipped with ghc-lib. These globs get glommed onto
-- the 'extraFiles' above as 'extra-source-files'.
cHeaders :: [String]
cHeaders =
  [ "includes/ghcconfig.h"
  , "includes/MachDeps.h"
  , "includes/CodeGen.Platform.hs"
  , "compiler/nativeGen/*.h"
  , "compiler/utils/*.h"
  , "compiler/*.h"
  ]

-- | Calculate via `ghc -M` the list of modules that are required for
-- 'ghc-lib-parser'.
calcParserModules :: IO [String]
calcParserModules = do
  lib <- mapM readCabalFile cabalFileLibraries
  let includeDirs = map ("-I" ++ ) ghcLibParserIncludeDirs
      hsSrcDirs = ghcLibParserHsSrcDirs lib
      hsSrcIncludes = map ("-i" ++ ) hsSrcDirs
      cmd = unwords $
        [ "stack exec -- ghc"
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
applyPatchDisableCompileTimeOptimizations :: IO ()
applyPatchDisableCompileTimeOptimizations =
    forM_ [ "compiler/main/DynFlags.hs"
          , "compiler/hsSyn/HsInstances.hs" ] $
    \file ->
        writeFile file .
        ("{-# OPTIONS_GHC -O0 #-}\n" ++)
        =<< readFile' file

-- | Stub out a couple of definitions in the ghc-heap library that
-- require CMM features, since Cabal doesn't work well with CMM files.
applyPatchHeapClosures :: IO ()
applyPatchHeapClosures = do
    let file = "libraries/ghc-heap/GHC/Exts/Heap/Closures.hs"
    writeFile file .
        replace
            "foreign import prim \"aToWordzh\" aToWord# :: Any -> Word#"
            "aToWord# :: Any -> Word#\naToWord# _ = 0##\n" .
        replace
            "foreign import prim \"reallyUnsafePtrEqualityUpToTag\"\n    reallyUnsafePtrEqualityUpToTag# :: Any -> Any -> Int#"
            "reallyUnsafePtrEqualityUpToTag# :: Any -> Any -> Int#\nreallyUnsafePtrEqualityUpToTag# _ _ = 0#\n"
        =<< readFile' file

-- | Fix up these rts include paths. We don't ship rts headers since
-- we run ghc-lib using the RTS of the compiler we build with - we go
-- to the compiler installation for those.
applyPatchRtsIncludePaths :: IO ()
applyPatchRtsIncludePaths =
    forM_ [ "compiler/cmm/SMRep.hs"
          , "compiler/codeGen/StgCmmLayout.hs" ] $
    \file ->
        writeFile file .
          replace
              "../includes/rts"
              "rts"
        =<< readFile' file

-- | Mangle exported C symbols to avoid collisions between the symbols
-- in ghc-lib-parser and ghc.
mangleCSymbols :: IO ()
mangleCSymbols = do
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
applyPatchStage :: IO ()
applyPatchStage =
    forM_ [ "compiler/ghci/Linker.hs"
          , "compiler/utils/FastString.hs"
          , "compiler/main/DynFlags.hs"] $
    \file ->
        writeFile file .
        replace "STAGE >= 2" "0" .
        replace "STAGE < 2" "1"
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
libBinParserModules :: IO ([Cabal], [Cabal], [String])
libBinParserModules = do
    lib <- mapM readCabalFile cabalFileLibraries
    bin <- readCabalFile cabalFileBinary
    parserModules <- calcParserModules
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
generateGhcLibCabal _ghcFlavor = do
    -- Compute the list of modules to be compiled. The rest are parser
    -- modules re-exported from ghc-lib-parser.
    (lib, bin, parserModules) <- libBinParserModules
    let nonParserModules =
          Set.toList (Set.difference
          (Set.fromList (askField lib "exposed-modules:" ))
          (Set.fromList parserModules))

    writeFile "ghc-lib.cabal" $ unlines $ map trimEnd $
        -- header
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
        ,"data-files:"] ++
        indent dataFiles ++
        ["extra-source-files:"] ++
        -- Remove Config.hs, Version.hs, Parser.hs and Lexer.hs from
        -- the list of extra source files here.
        indent (reverse (drop 4 $ reverse extraFiles)) ++ indent cHeaders ++
        ["tested-with: GHC==8.6.3, GHC==8.4.3"
        ,"source-repository head"
        ,"    type: git"
        ,"    location: git@github.com:digital-asset/ghc-lib.git"
        ,""
        ,"library"
        ,"    default-language:   Haskell2010"
        ,"    default-extensions: NoImplicitPrelude"
        ,"    include-dirs:"] ++
        indent2 ghcLibIncludeDirs ++
        ["    ghc-options: -fobject-code -package=ghc-boot-th -optc-DTHREADED_RTS"
        ,"    cc-options: -DTHREADED_RTS"
        ,"    cpp-options: -DSTAGE=2 -DTHREADED_RTS -DGHCI -DGHC_IN_GHCI"
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
        indent2 (ghcLibHsSrcDirs lib) ++
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

-- | Produces a ghc-lib-parser Cabal file.
generateGhcLibParserCabal :: GhcFlavor -> IO ()
generateGhcLibParserCabal _ghcFlavor = do
    (lib, bin, parserModules) <- libBinParserModules
    writeFile "ghc-lib-parser.cabal" $ unlines $ map trimEnd $
        -- header
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
        ,"data-files:"] ++
        indent dataFiles ++
        ["extra-source-files:"] ++
        indent extraFiles ++ indent cHeaders ++
        ["tested-with: GHC==8.6.3, GHC==8.4.3"
        ,"source-repository head"
        ,"    type: git"
        ,"    location: git@github.com:digital-asset/ghc-lib.git"
        ,""
        ,"library"
        ,"    default-language:   Haskell2010"
        ,"    default-extensions: NoImplicitPrelude"
        ,"    include-dirs:"] ++
        indent2 ghcLibParserIncludeDirs ++
        ["    ghc-options: -fobject-code -package=ghc-boot-th -optc-DTHREADED_RTS"
        ,"    cc-options: -DTHREADED_RTS"
        ,"    cpp-options: -DSTAGE=2 -DTHREADED_RTS -DGHCI -DGHC_IN_GHCI"
        ,"    if !os(windows)"
        ,"        build-depends: unix"
        ,"    else"
        ,"        build-depends: Win32"
        ,"    build-depends:"]++
        indent2 (withCommas commonBuildDepends) ++
        ["    build-tools: alex >= 3.1, happy >= 1.19.4"
        ,"    other-extensions:"] ++
        indent2 (askField lib "other-extensions:") ++
        ["    c-sources:"] ++
        -- We hardcode these because the inclusion of 'keepCAFsForGHCi'
        -- causes issues in ghci see
        -- https://github.com/digital-asset/ghc-lib/issues/27
        indent2 ["compiler/cbits/genSym.c","compiler/parser/cutils.c"] ++
        ["    hs-source-dirs:"] ++
        indent2 (ghcLibParserHsSrcDirs lib) ++
        ["    autogen-modules:"
        ,"        Lexer"
        ,"        Parser"
        ] ++
        ["    exposed-modules:"
        ]++ indent2 parserModules
    removeGeneratedIntermediateFiles
    putStrLn "# Generating 'ghc-lib-parser.cabal'... Done!"

-- | Run Hadrian to build the things that the Cabal files need.
generatePrerequisites :: IO ()
generatePrerequisites = do
  system_ "stack build alex happy" -- If building happy from git, the
                                   -- next line can fail without this.
  system_ "stack --stack-yaml hadrian/stack.yaml build --only-dependencies"
  system_ "stack --stack-yaml hadrian/stack.yaml exec -- bash -c ./boot"
  system_ "stack --stack-yaml hadrian/stack.yaml exec -- bash -c \"./configure --enable-tarballs-autodownload\""
  withCurrentDirectory "hadrian" $ do
    system_ "stack build --no-library-profiling"
    system_ $ unwords $
        ["stack exec hadrian --"
        ,"--directory=.."
        ,"--integer-simple"
        ,"--build-root=ghc-lib"
        ] ++ extraFiles ++
        map (dataDir </>) dataFiles
  -- We use the hadrian generated Lexer and Parser so get these out
  -- of the way.
  removeFile "compiler/parser/Lexer.x"
  removeFile "compiler/parser/Parser.y"
  removeFile "compiler/utils/Fingerprint.hsc" -- Favor the generated .hs file here too.
