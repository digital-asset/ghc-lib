-- -*- truncate-lines: t; -*-

{- Prep. for building GHC as a library. -}

import System.Environment
import System.Process.Extra
import System.FilePath hiding ((</>))
import System.FilePath.Posix((</>)) -- make sure we generate / on all platforms
import System.Directory
import System.IO.Extra
import Control.Monad
import Data.List.Extra
import Data.Char

-- Constants.

-- | Cabal files from libraries inside GHC that are merged together
cabalFileLibraries :: [FilePath]
cabalFileLibraries =
    ["libraries/template-haskell/template-haskell.cabal"
    ,"libraries/ghc-heap/ghc-heap.cabal"
    ,"libraries/ghc-boot-th/ghc-boot-th.cabal"
    ,"libraries/ghc-boot/ghc-boot.cabal"
    ,"libraries/ghci/ghci.cabal"
    ,"compiler/ghc.cabal"
    ]

-- | Cabal file for the GHC binary
cabalFileBinary :: FilePath
cabalFileBinary = "ghc/ghc-bin.cabal"

-- | Additional source and data files for Cabal.
--   The files in this list are all created by Hadrian.
extraFiles :: [FilePath]
extraFiles =
    -- source files
    ["ghc-lib/generated/ghcautoconf.h"
    ,"ghc-lib/generated/ghcplatform.h"
    ,"ghc-lib/generated/ghcversion.h"
    ,"ghc-lib/generated/DerivedConstants.h"
    ,"ghc-lib/generated/GHCConstantsHaskellExports.hs"
    ,"ghc-lib/generated/GHCConstantsHaskellType.hs"
    ,"ghc-lib/generated/GHCConstantsHaskellWrappers.hs"
    ,"ghc-lib/stage1/compiler/build/ghc_boot_platform.h"
    ,"ghc-lib/stage1/compiler/build/primop-can-fail.hs-incl"
    ,"ghc-lib/stage1/compiler/build/primop-code-size.hs-incl"
    ,"ghc-lib/stage1/compiler/build/primop-commutable.hs-incl"
    ,"ghc-lib/stage1/compiler/build/primop-data-decl.hs-incl"
    ,"ghc-lib/stage1/compiler/build/primop-fixity.hs-incl"
    ,"ghc-lib/stage1/compiler/build/primop-has-side-effects.hs-incl"
    ,"ghc-lib/stage1/compiler/build/primop-list.hs-incl"
    ,"ghc-lib/stage1/compiler/build/primop-out-of-line.hs-incl"
    ,"ghc-lib/stage1/compiler/build/primop-primop-info.hs-incl"
    ,"ghc-lib/stage1/compiler/build/primop-strictness.hs-incl"
    ,"ghc-lib/stage1/compiler/build/primop-tag.hs-incl"
    ,"ghc-lib/stage1/compiler/build/primop-vector-tycons.hs-incl"
    ,"ghc-lib/stage1/compiler/build/primop-vector-tys-exports.hs-incl"
    ,"ghc-lib/stage1/compiler/build/primop-vector-tys.hs-incl"
    ,"ghc-lib/stage1/compiler/build/primop-vector-uniques.hs-incl"
    ,"ghc-lib/stage1/compiler/build/Config.hs"
    ]

-- |'dataDir' is the directory cabal looks for data files to install,
-- relative to the source directory.
dataDir :: FilePath
dataDir = "ghc-lib/stage1/lib"

-- |'dataFiles' is a list of files to be installed for run-time use by
-- the package.
dataFiles :: [FilePath]
dataFiles =
    ["settings"
    ,"llvm-targets"
    ,"llvm-passes"
    ,"platformConstants"
    ]

-- |'appPatchHeapClosures' stubs out a couple of definitions on a
-- particular file in the ghc-heap library.
appPatchHeapClosures :: IO ()
appPatchHeapClosures = do
    let file = "libraries/ghc-heap/GHC/Exts/Heap/Closures.hs"
    writeFile file .
        replace
            "foreign import prim \"aToWordzh\" aToWord# :: Any -> Word#"
            "aToWord# :: Any -> Word#\naToWord# _ = 0##\n" .
        replace
            "foreign import prim \"reallyUnsafePtrEqualityUpToTag\"\n    reallyUnsafePtrEqualityUpToTag# :: Any -> Any -> Int#"
            "reallyUnsafePtrEqualityUpToTag# :: Any -> Any -> Int#\nreallyUnsafePtrEqualityUpToTag# _ _ = 0#\n"
        =<< readFile' file

-- Functions for generating files.

data Cabal = Cabal
    {cabalDir :: FilePath -- the directory this file exists in
    ,cabalFields :: [(String, [String])] -- the key/value pairs it contains
    }

-- | Given a file, produce the key/value pairs it contains
readCabalFile :: FilePath -> IO Cabal
readCabalFile file = do
    src <- readFile' file
    let fields = repeatedly f $ wordsBy (\x -> isSpace x || x == ',') $ unlines $ filter (not . isIf) $ map trimComment $ lines src
    return $ Cabal (takeDirectory file) fields
    where
        isIf x = "if " `isPrefixOf` trim x
        trimComment x = maybe x fst $ stripInfix "--" x
        f (x:xs) = let (a,b) = break (":" `isSuffixOf`) xs in ((lower x,a),b)

askCabalField :: Cabal -> String -> [String]
askCabalField cbl x = concatMap snd $ filter ((==) x . fst) $ cabalFields cbl

askCabalFiles :: Cabal -> String -> [String]
askCabalFiles cbl x = map (cabalDir cbl </>) $ askCabalField cbl x


-- |'withCabals' folds a function over the set of cabal files.
withCabals :: (Cabal -> [String]) -> IO [String]
withCabals f = fmap (nubSort . concat) $ forM cabalFileLibraries $ \file -> do
    f <$> readCabalFile file

-- |'modules' extracts a list of modules (e.g. "exposed-modules") from
-- the set of cabal files.
modules :: String -> IO [String]
modules prim = withCabals $ \cbl -> askCabalField cbl prim


-- |'otherExtensions' extracts a list of "other-extensions" from the
-- set of cabal files (playing a bit fast and loose here to be honest).
otherExtensions :: IO [String]
otherExtensions = withCabals $ \cbl -> askCabalField cbl "other-extensions:"


-- |'cSrcs' extracts a list of C source files (i.e. "c-sources") from
-- the set of cabal files.
cSrcs :: IO [String]
cSrcs = withCabals $ \cbl -> askCabalFiles cbl "c-sources:"

-- |'cmmSrcs' extracts a list of C-- source files (i.e. "cmm-sources")
-- from the set of cabal files.
cmmSrcs :: IO [String]
cmmSrcs = withCabals $ \cbl -> askCabalFiles cbl "cmm-sources:"

-- |'hsSourceDirs' extracts a list of source directories from the set
-- of cabal files.
hsSourceDirs :: IO [String]
hsSourceDirs = withCabals $ \cbl -> cabalDir cbl : askCabalFiles cbl "hs-source-dirs:"


-- |'exeOtherExtensions' extracts a list of "other-extensions" from the GHC
-- as an exe cabal file.
exeOtherExtensions :: IO [String]
exeOtherExtensions = do
  cbl <- readCabalFile cabalFileBinary
  return $ askCabalField cbl "other-extensions:"

-- |'exeOtherModules' extracts a list of "other-modules" from the GHC
-- as an exe cabal file.
exeOtherModules = do
  cbl <- readCabalFile cabalFileBinary
  return $ askCabalField cbl "other-modules:"

-- |'genCabal' produces a cabal file for ghc with supporting
-- libraries.
genCabal = do
    src <- hsSourceDirs
    ems <- modules "exposed-modules:"
    oms <- modules "other-modules:"
    csf <- cSrcs
    cmm <- cmmSrcs
    oxt <- otherExtensions
    eoe <- exeOtherExtensions
    eom <- exeOtherModules
    let indent = map ("    "++)
    let indent2 = indent . indent
    writeFile "ghc-lib.cabal" $ unlines $ map trimEnd $
        -- header
        ["cabal-version: 2.1" -- or cabal check complains about cmm-sources
        ,"build-type: Simple"
        ,"name: ghc-lib"
        ,"version: 0.1.0"
        ,"license: BSD-3-Clause"
        ,"license-file: LICENSE"
        ,"category: Development"
        ,"author: The GHC Team and Digital Asset"
        ,"maintainer: Digital Asset"
        ,"synopsis: The GHC API, not tied to GHC versions"
        ,"description: A package equivalent to the @ghc@ package, but which can be loaded on many compiler versions."
        ,"homepage: https://github.com/digital-asset/ghc-lib"
        ,"bug-reports: https://github.com/digital-asset/ghc-lib/issues"
        ,"data-dir: " ++ dataDir
        ,"data-files:"] ++
        indent dataFiles ++
        ["extra-source-files:"] ++
        indent extraFiles ++
        ["    includes/*.h"
        ,"    includes/CodeGen.Platform.hs"
        ,"    includes/rts/*.h"
        ,"    includes/rts/storage/*.h"
        ,"    includes/rts/prof/*.h"
        ,"    compiler/nativeGen/*.h"
        ,"    compiler/utils/*.h"
        ,"    compiler/*.h"
        ,"tested-with:GHC==8.4.3"
        ,"source-repository head"
        ,"    type: git"
        ,"    location: git://git.haskell.org/ghc.git"
        ,""
        ,"library"
        ,"    default-language:   Haskell2010"
        ,"    default-extensions: NoImplicitPrelude"
        ,"    include-dirs:"
        ,"        ghc-lib/generated"
        ,"        ghc-lib/stage1/compiler/build"
        ,"        compiler"
        ,"        compiler/utils"
        ,"    ghc-options: -fobject-code -package=ghc-boot-th -optc-DTHREADED_RTS"
        ,"    cc-options: -DTHREADED_RTS"
        ,"    cpp-options: -DSTAGE=2 -DTHREADED_RTS -DGHCI -DGHC_IN_GHCI"
        ,"    if !os(windows)"
        ,"        build-depends: unix"
        ,"    else"
        ,"        build-depends: Win32"
        ,"    build-depends:"
        ,"        ghc-prim,"
        ,"        base == 4.*, containers, bytestring, binary,"
        ,"        filepath, directory, array, deepseq,"
        ,"        pretty, time, transformers, process, haskeline, hpc"
        ,"    build-tools: alex >= 3.1, happy >= 1.19.4"
        ,"    other-extensions:"] ++
        indent2 oxt ++
        ["    c-sources:"] ++
        indent2 csf ++
        ["    cmm-sources:"] ++
        indent2 cmm ++
        ["    hs-source-dirs:"
        ,"        ghc-lib/stage1/compiler/build"] ++
        indent2 src ++
        ["    exposed-modules:"] ++
        indent2 ems ++
        ["    other-modules:"] ++
        indent2 oms ++
        [""
        ,"executable ghc-lib"
        ,"    default-language:   Haskell2010"
        ,"    if !os(windows)"
        ,"        build-depends: unix"
        ,"    else"
        ,"        build-depends: Win32"
        ,"    build-depends:"
        ,"        base == 4.*, array, bytestring, directory, process, filepath,"
        ,"        containers, deepseq, ghc-prim, haskeline, time, transformers,"
        ,"        ghc-lib"
        ,"    hs-source-dirs: ghc"
        ,"    ghc-options: -fobject-code -package=ghc-boot-th -optc-DTHREADED_RTS"
        ,"    cc-options: -DTHREADED_RTS"
        ,"    cpp-options: -DGHCI -DTHREADED_RTS -DGHC_LOADED_INTO_GHCI"
        ,"    other-modules:"] ++
        indent2 eom ++
        ["    other-extensions:"] ++
        indent2 eoe ++
        ["    default-extensions: NoImplicitPrelude"
        ,"    main-is: Main.hs"
        ]

genPrerequisites :: IO ()
genPrerequisites =
  withCurrentDirectory "hadrian" $ do
    system_ "stack build --no-library-profiling"
    system_ $ unwords $
      ["stack exec hadrian --"
      ,"--directory=.."
      ,"--configure"
      ,"--integer-simple"
      ,"--build-root=ghc-lib"
      ] ++ extraFiles ++
      map (dataDir </>) dataFiles

-- Driver.

-- | 'main' expects a single argument, the root of a GHC source tree.
main :: IO ()
main = do
  [root] <- getArgs
  withCurrentDirectory root $ do
    appPatchHeapClosures
    genPrerequisites
    genCabal
