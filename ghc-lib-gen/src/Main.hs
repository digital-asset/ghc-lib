
-- | Generate a ghc-lib.cabal package given a GHC directory
module Main(main) where

import System.Environment
import System.Process.Extra
import System.FilePath hiding ((</>))
import System.FilePath.Posix((</>)) -- make sure we generate / on all platforms
import System.Directory
import System.IO.Extra
import Data.List.Extra
import Data.Char


main :: IO ()
main = do
    xs <- getArgs
    case xs of
        [root] -> withCurrentDirectory root $ do
            applyPatchHeapClosures
            generatePrerequisites
            generateCabal
        _ -> fail "You must pass exactly 1 argument, the directory containing the ghc source code."

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

-- | Stub out a couple of definitions in the ghc-heap library that require CMM features,
--   since Cabal doesn't work well with CMM files.
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


-- | Data type representing an approximately parsed Cabal file
data Cabal = Cabal
    {cabalDir :: FilePath -- the directory this file exists in
    ,cabalFields :: [(String, [String])] -- the key/value pairs it contains
    }

-- | Given a file, produce the key/value pairs it contains (approximate but good enough)
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

-- | Ask a Cabal file for a file, which is relative to the underlying Cabal file.
askCabalFiles :: Cabal -> String -> [String]
askCabalFiles cbl x = map (cabalDir cbl </>) $ askCabalField cbl x



-- | Produces the Cabal file.
generateCabal :: IO ()
generateCabal = do
    lib <- mapM readCabalFile cabalFileLibraries
    bin <- (:[]) <$> readCabalFile cabalFileBinary
    let askField from x = nubSort $ concatMap (`askCabalField` x) from
    let askFiles from x = nubSort $ concatMap (`askCabalFiles` x) from

    let indent = map ("    "++)
    let indent2 = indent . indent
    writeFile "ghc-lib.cabal" $ unlines $ map trimEnd $
        -- header
        ["cabal-version: 2.2" -- or cabal check complains about cmm-sources
        ,"build-type: Simple"
        ,"name: ghc-lib"
        ,"version: 0.1.0"
        ,"license: BSD-3-Clause"
        ,"license-file: LICENSE"
        ,"category: Development"
        ,"author: The GHC Team and Digital Asset"
        ,"maintainer: Digital Asset"
        ,"synopsis: The GHC API, decoupled from GHC versions"
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
        ,"        ghc-prim > 0.2 && < 0.6,"
        ,"        base == 4.11 && < 4.14,"
        ,"        containers >= 0.5 && < 0.7,"
        ,"        bytestring >= 0.9 && < 0.11,"
        ,"        binary == 0.8.*,"
        ,"        filepath >= 1 && < 1.5,"
        ,"        directory >= 1 && < 1.4,"
        ,"        array >= 0.1 && < 0.6,"
        ,"        deepseq >= 1.4 && < 1.5,"
        ,"        pretty == 1.1.*,"
        ,"        time >= 1.4 && < 1.10,"
        ,"        transformers == 0.5.*,"
        ,"        process >= 1 && < 1.7,"
        ,"        hpc == 0.6.*"
        ,"    build-tools: alex >= 3.1, happy >= 1.19.4"
        ,"    other-extensions:"] ++
        indent2 (askField lib "other-extensions:") ++
        ["    c-sources:"] ++
        indent2 (askFiles lib "c-sources:") ++
        ["    cmm-sources:"] ++
        indent2 (askFiles lib "cmm-sources:") ++
        ["    hs-source-dirs:"] ++
        indent2 (nubSort $
            "ghc-lib/stage1/compiler/build" :
            map takeDirectory cabalFileLibraries ++
            askFiles lib "hs-source-dirs:") ++
        ["    autogen-modules:"
        ,"        Paths_ghc_lib"] ++
        ["    exposed-modules:"
        ,"        Paths_ghc_lib"] ++
        indent2 (askField lib "exposed-modules:") ++
        ["    other-modules:"] ++
        indent2 (askField lib "other-modules:") ++
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
        indent2 (askField bin "other-modules:") ++
        ["    other-extensions:"] ++
        indent2 (askField bin "other-extensions:") ++
        ["    default-extensions: NoImplicitPrelude"
        ,"    main-is: Main.hs"
        ]

-- | Run Hadrian to build the things that the Cabal file needs
generatePrerequisites :: IO ()
generatePrerequisites = withCurrentDirectory "hadrian" $ do
    system_ "stack build --no-library-profiling"
    system_ $ unwords $
        ["stack exec hadrian --"
        ,"--directory=.."
        ,"--configure"
        ,"--integer-simple"
        ,"--build-root=ghc-lib"
        ] ++ extraFiles ++
        map (dataDir </>) dataFiles
