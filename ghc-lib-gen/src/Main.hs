-- -*- truncate-lines: t; -*-

{- Prep. for building GHC as a library. -}

import System.Environment
import System.Info.Extra
import System.Process.Extra
import System.FilePath
import System.Directory
import System.IO.Extra
import Control.Monad
import Data.Maybe
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
    -- data files
    ,"ghc-lib/stage1/lib/settings"
    ,"ghc-lib/stage1/lib/llvm-targets"
    ,"ghc-lib/stage1/lib/llvm-passes"
    ,"ghc-lib/stage1/lib/platformConstants"
    ]

-- |'dataDir' is the directory cabal looks for data files to install,
-- relative to the source directory.
dataDir :: FilePath
dataDir = "ghc-lib/stage1/lib"

-- |'dataFiles' is a list of files to be installed for run-time use by
-- the package.
dataFiles =
  [
    "settings"
  , "llvm-targets"
  , "llvm-passes"
  , "platformConstants"
  ]

-- |'appPatchHeapClosures' stubs out a couple of definitions on a
-- particular file in the ghc-heap library.
appPatchHeapClosures :: FilePath -> IO ()
appPatchHeapClosures root =
  do
    s <- readFile' src
    writeFile src $ fromMaybe s ((fixaToWord >=> fixReallyUnsafePtrEqualityUpToTag) s)
  where
      src = root </> "libraries/ghc-heap/GHC/Exts/Heap/Closures.hs"
      aToWord ="foreign import prim \"aToWordzh\" aToWord# :: Any -> Word#"
      reallyUnsafePtrEqualityUpToTag="foreign import prim \"reallyUnsafePtrEqualityUpToTag\"\n    reallyUnsafePtrEqualityUpToTag# :: Any -> Any -> Int#"

      fixaToWord s =
        case stripInfix aToWord s of
          Nothing -> Nothing
          Just (b, rest) ->
            Just (b ++ "aToWord# :: Any -> Word#\naToWord# _ = 0##\n" ++ rest)

      fixReallyUnsafePtrEqualityUpToTag s =
        case stripInfix reallyUnsafePtrEqualityUpToTag s of
          Nothing -> Nothing
          Just (b, rest) ->
            Just (b
                  ++ "reallyUnsafePtrEqualityUpToTag# :: Any -> Any -> Int#\nreallyUnsafePtrEqualityUpToTag# _ _ = 0#\n" ++ rest)

-- Functions for generating files.

-- |'harvest' finds all entries for a given section in the text of a
-- cabal file.
harvest :: String -> [String] -> String -> [String]
harvest tag acc s =
  let reallyHarvest s =
        let ls = lines s in
          let ms =
                map
                (dropWhile isSpace)
                (takeWhile (':' `notElem`) ls) in
            harvest tag (acc ++ ms) s in
    case stripInfix tag s of
      Nothing -> acc
      Just (_, '\n' : after) -> reallyHarvest after
      Just (_, after) -> reallyHarvest after

-- |'withCabals' folds a function over the set of cabal files.
withCabals :: (String -> String -> [String]) -> String -> IO [String]
withCabals f root =
  foldM fun [] (map (root </>) cabalFileLibraries) where
  fun acc c = do
      s <- readFile' c
      return (f c s ++ acc)

-- |'modules' extracts a list of modules (e.g. "exposed-modules") from
-- the set of cabal files.
modules :: String -> String -> String -> IO [String]
modules prim alt =
  withCabals (\_ s -> nubOrd (harvest prim (harvest alt [] s) s))


-- |'otherExtensions' extracts a list of "other-extensions" from the
-- set of cabal files (playing a bit fast and loose here to be honest).
otherExtensions :: String -> IO [String]
otherExtensions root = foldM fun [] (map (root </>)
      -- This particular file has ',' and its extensions are
      -- covered elsewhere.
      (delete "libraries/ghc-boot/ghc-boot.cabal" cabalFileLibraries))
    where
        fun acc c = do
                 s <- readFile' c
                 return (nubOrd (f c s ++ acc))

        f file s =
          filter
            (\l -> not (null l || "--" `isPrefixOf` l
                              || "if flag" `isPrefixOf` l ))
              (harvest "other-extensions:"
              (harvest "Other-Extensions:" [] s) s)


-- |'cSrcs' extracts a list of C source files (i.e. "c-sources") from
-- the set of cabal files.
cSrcs :: String -> IO [String]
cSrcs root = withCabals f root
    where
        f file s =
          let dir = map (\x -> if isPathSeparator x then '/' else x) $ takeDirectory file
                in map (\f ->
                          fromMaybe "" (stripPrefix (root ++ "/") (dir </> f))
                      ) fs
            where fs = filter
                    (\l -> not (null l || "--" `isPrefixOf` l))
                    (nubOrd (harvest "c-sources:"
                                (harvest "C-Sources:" [] s) s))

-- |'cmmSrcs' extracts a list of C-- source files (i.e. "cmm-sources")
-- from the set of cabal files.
cmmSrcs :: String -> IO [String]
cmmSrcs root =
  let f file s =
        let dir = map (\x -> if isPathSeparator x then '/' else x) $ takeDirectory file
              in map (\f ->
                         fromMaybe "" (stripPrefix (root ++ "/") (dir </> f))
                 ) fs
        where fs = filter
                (\l -> not (null l || "--" `isPrefixOf` l))
                (nubOrd (harvest "cmm-sources:"
                             (harvest "Cmm-Sources:" [] s) s))
  in withCabals f root

-- |'hsSourceDirs' extracts a list of source directories from the set
-- of cabal files.
hsSourceDirs :: String -> IO [String]
hsSourceDirs root =
  let f file s =
        let dir = map (\x -> if isPathSeparator x then '/' else x) $ takeDirectory file
            dir' = fromMaybe "" (stripPrefix (root ++ "/") dir)
              in dir' : map (\f ->
                            fromMaybe "" (stripPrefix (root ++ "/") (dir ++ "/" ++ f))
                           ) fs
        where fs = filter
                (\l -> not (null l || "--" `isPrefixOf` l))
                (nubOrd (harvest "hs-source-dirs:"
                             (harvest "Hs-Source-Dirs:" [] s) s))
  in do
       files <- withCabals f root
       return $ "ghc-lib/stage1/compiler/build" : files


-- |'exeOtherExtensions' extracts a list of "other-extensions" from the GHC
-- as an exe cabal file.
exeOtherExtensions :: String -> IO [String]
exeOtherExtensions root = do
  s <- readFile' $ root </> cabalFileBinary
  return $ filter
             (\l -> not (null l || "--" `isPrefixOf` l
                                || "if flag" `isPrefixOf` l ))
             (nubOrd (harvest "other-extensions:"
                 (harvest "Other-Extensions:" [] s) s))

-- |'exeOtherModules' extracts a list of "other-modules" from the GHC
-- as an exe cabal file.
exeOtherModules root = do
  s <- readFile' $ root </> cabalFileBinary
  return $ filter
             (\l -> not (null l || "--" `isPrefixOf` l))
             (nubOrd (harvest "other-modules:"
             (harvest "Other-Modules:" [] s) s))

-- |'genCabal' produces a cabal file for ghc with supporting
-- libraries.
genCabal root = do
  src <- hsSourceDirs root
  ems <- modules "exposed-modules:" "Exposed-Modules:" root
  oms <- modules "other-modules:" "Other-Modules:" root
  csf <- cSrcs root
  cmm <- cmmSrcs root
  oxt <- otherExtensions root
  eoe <- exeOtherExtensions root
  eom <- exeOtherModules root
  let src' = map (\x -> "      " ++ x ++ "\n") src
      ems' = map (\x -> "      " ++ x ++ "\n") ems
      oms' = map (\x -> "      " ++ x ++ "\n") oms
      csf' = map (\x -> "      " ++ x ++ "\n") csf
      cmm' = map (\x -> "      " ++ x ++ "\n") cmm
      oxt' = map (\x -> "      " ++ x ++ "\n") oxt
      eoe' = map (\x -> "      " ++ x ++ "\n") eoe
      eom' = map (\x -> "      " ++ x ++ "\n") eom
      dat' = map (\x -> "  " ++ x ++ "\n") dataFiles
      ext' = map (\x -> "  " ++ x ++ "\n")
                -- Tiny bit of monkey business here where we separate
                -- data files from genuine source files.
                (let fs = map (\f -> dataDir </> f) dataFiles in
                            foldl (flip delete) extraFiles fs)
  let contents =
       unlines [

          -- header

          "cabal-version: 2.1" -- or cabal check complains about cmm-sources
        , "build-type: Simple"
        , "name: ghc-lib"
        , "version: 0.1.0"
        , "license: BSD-3-Clause"
        , "license-file: LICENSE"
        , "category: Development"
        , "author: XXX"
        , "maintainer: XXX"
        , "copyright: XXX"
        , "synopsis: GHC as a library"
        , "description: GHC as a library that can be loaded into GHCi."
        , "homepage: XXX"
        , "bug-reports: XXX"
        , "data-dir: " ++ dataDir
        , "data-files:\n" ++ concat dat'
        , "extra-source-files:\n" ++ concat ext'
                                  ++ "  includes/*.h\n"
                                  ++ "  includes/CodeGen.Platform.hs\n"
                                  ++ "  includes/rts/*.h\n"
                                  ++ "  includes/rts/storage/*.h\n"
                                  ++ "  includes/rts/prof/*.h\n"
                                  ++ "  compiler/nativeGen/*.h\n"
                                  ++ "  compiler/utils/*.h\n"
                                  ++ "  compiler/*.h\n"
        , "tested-with:GHC==8.4.3"
        , "source-repository head"
        , "    type: git"
        , "    location: git://git.haskell.org/ghc.git"

        -- library

        , "library"
        , "    default-language:   Haskell2010"
        , "    default-extensions: NoImplicitPrelude\n"
        , "    include-dirs:"
        , "      ghc-lib/generated"
        , "      ghc-lib/stage1/compiler/build"
        , "      compiler"
        , "      compiler/utils"
        , "    ghc-options: -fobject-code -package=ghc-boot-th -optc-DTHREADED_RTS"
        , "    cc-options: -DTHREADED_RTS"
        , "    cpp-options: -DSTAGE=2 -DTHREADED_RTS -DGHCI -DGHC_IN_GHCI"
        , "    if !os(windows)"
        , "        build-depends: unix"
        , "    else"
        , "        build-depends: Win32"
        , "    build-depends:"
        , "      ghc-prim"
        , "      , base == 4.*, containers, bytestring, binary"
        , "      , filepath, directory, array, deepseq"
        , "      , pretty, time, transformers, process, haskeline, hpc"
        , "    build-tools: alex >= 3.1 , happy >= 1.19.4"
        ]
       ++ "    other-extensions:\n"   ++ concat oxt'
       ++ "    c-sources:\n"          ++ concat csf'
       ++ "    cmm-sources:\n"        ++ concat cmm'
       ++ "    hs-source-dirs:\n"     ++ concat src'
       ++ "    exposed-modules:\n"    ++ concat ems'
       ++ "    other-modules:\n"      ++ concat oms'
       ++ "\n"

       -- exe

       ++ unlines [
          "executable ghc-lib"
        , "    default-language:   Haskell2010"
        , "    if !os(windows)"
        , "        build-depends: unix"
        , "    else"
        , "        build-depends: Win32"
        , "    build-depends:"
        , "        base == 4.*, array, bytestring, directory, process, filepath,"
        , "        containers, deepseq, ghc-prim, haskeline, time, transformers,"
        , "        ghc-lib"
        , "    hs-source-dirs: ghc"
        , "    ghc-options: -fobject-code -package=ghc-boot-th -optc-DTHREADED_RTS"
        , "    cc-options: -DTHREADED_RTS"
        , "    cpp-options: -DGHCI -DTHREADED_RTS -DGHC_LOADED_INTO_GHCI"
        ]
       ++ "    other-modules:\n" ++ concat eom'
       ++ "    other-extensions:\n" ++ concat eoe'
       ++ unlines [
          "    default-extensions: NoImplicitPrelude"
        , "    main-is: Main.hs"
        ]
  writeFile (root </> "ghc-lib.cabal") contents

genPrerequisites :: String -> IO ()
genPrerequisites root = do
  -- FIXME: We want to use Cabal, but it has issues with Alex we have difficulty with
  when False $
    withCurrentDirectory root $
      system_ $ unwords $
        ["hadrian" </> "build.cabal." ++ (if isWindows then "bat" else "sh")
        ,"--configure"
        ,"--integer-simple"
        ,"--build-root=ghc-lib"
        ] ++ extraFiles

  withCurrentDirectory (root </> "hadrian") $ do
    system_ "stack build --no-library-profiling"
    system_ $ unwords $
      ["stack exec hadrian --"
      ,"--directory=.."
      ,"--configure"
      ,"--integer-simple"
      ,"--build-root=ghc-lib"
      ] ++ extraFiles

-- Driver.

-- | 'main' expects a single argument, the root of a GHC source tree.
main :: IO ()
main = do
  [root] <- getArgs
  appPatchHeapClosures root
  genPrerequisites root
  genCabal root
