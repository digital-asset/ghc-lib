-- Copyright (c) 2019, Digital Asset (Switzerland) GmbH and/or its
-- affiliates. All rights reserved.  SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

-- CI script, compatible with all of Travis, Appveyor and Azure.

import Control.Monad
import Control.Exception
import System.Directory
import System.FilePath
import System.IO.Error
import System.IO.Extra
import System.Info.Extra
import System.Process.Extra
import System.Time.Extra
import Data.List

main :: IO ()
main = do
    when isWindows $
        cmd "stack exec -- pacman -S autoconf automake-wrapper make patch python tar --noconfirm"

    -- Clear up any detritus left over from previous runs (useful when
    -- building locally).
    rmDirs ["ghc", "ghc-lib-parser", "ghc-lib"]
    rmFiles <$> filter (isExtensionOf ".tar.gz") <$> getDirectoryContents "."
    cmd "git checkout stack.yaml"

    cmd "git clone https://gitlab.haskell.org/ghc/ghc.git"
    cmd "cd ghc && git checkout 4854a3490518760405f04826df1768b5a7b96da2" -- 07/21/2019
    cmd "cd ghc && git submodule update --init --recursive"

    appendFile "ghc/hadrian/stack.yaml" $ unlines ["ghc-options:","  \"$everything\": -O0 -j"]
    -- Build ghc-lib-gen. Do this here rather than in the Azure script
    -- so that it's not forgotten when testing this program locally.
    cmd "stack --no-terminal build"
    -- Make and extract an sdist of ghc-lib-parser.
    cmd "stack exec -- ghc-lib-gen ghc --ghc-lib-parser"
    mkTarball "ghc-lib-parser-0.1.0"
    renameDirectory "ghc-lib-parser-0.1.0" "ghc-lib-parser"
    removeFile "ghc/ghc-lib-parser.cabal"
    cmd "git checkout stack.yaml"

    cmd "cd ghc && git checkout ."
    appendFile "ghc/hadrian/stack.yaml" $ unlines ["ghc-options:","  \"$everything\": -O0 -j"]

    -- Make and extract an sdist of ghc-lib.
    cmd "stack exec -- ghc-lib-gen ghc --ghc-lib"
    tarball <- mkTarball "ghc-lib-0.1.0"
    renameDirectory "ghc-lib-0.1.0" "ghc-lib"
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
              ]

    -- Separate the two library build commands so they are
    -- independently timed. Note that optimizations in these builds
    -- are disabled in stack.yaml via `ghc-options: -O0`.
    cmd "stack build ghc-lib-parser --no-terminal --interleaved-output"
    cmd "stack build ghc-lib --no-terminal --interleaved-output"
    cmd "stack build mini-hlint mini-compile strip-locs --no-terminal --interleaved-output"
    -- Run tests.
    cmd "stack exec --no-terminal -- ghc-lib --version"
    cmd "stack exec --no-terminal -- mini-hlint examples/mini-hlint/test/MiniHlintTest.hs"
    cmd "stack exec --no-terminal -- mini-hlint examples/mini-hlint/test/MiniHlintTest_fatal_error.hs"
    cmd "stack exec --no-terminal -- mini-hlint examples/mini-hlint/test/MiniHlintTest_non_fatal_error.hs"
    cmd "stack exec --no-terminal -- mini-hlint examples/mini-hlint/test/MiniHlintTest_respect_dynamic_pragma.hs"
    cmd "stack exec --no-terminal -- mini-hlint examples/mini-hlint/test/MiniHlintTest_fail_unknown_pragma.hs"
    cmd "stack exec --no-terminal -- strip-locs examples/mini-compile/test/MiniCompileTest.hs"
    cmd "stack exec --no-terminal -- mini-compile examples/mini-compile/test/MiniCompileTest.hs"
    -- Test everything loads in GHCi, see
    -- https://github.com/digital-asset/ghc-lib/issues/27
    cmd "stack exec --no-terminal -- ghc -package=ghc-lib-parser -e \"print 1\""
    cmd "stack exec --no-terminal -- ghc -package=ghc-lib -e \"print 1\""
    where
      dropExtensions :: String -> String
      dropExtensions = dropExtension . dropExtension

      cmd :: String -> IO ()
      cmd x = do
        putStrLn $ "\n\n# Running: " ++ x
        hFlush stdout
        (t, _) <- duration $ system_ x
        putStrLn $ "# Completed in " ++ showDuration t ++ ": " ++ x ++ "\n"
        hFlush stdout

      mkTarball :: String -> IO ()
      mkTarball target = do
        stackYaml <- readFile' "stack.yaml"
        writeFile "stack.yaml" $ stackYaml ++ unlines ["- ghc"]
        sDistCreateExtract target

      sDistCreateExtract :: String -> IO ()
      sDistCreateExtract target = do
        cmd "stack sdist ghc --tar-dir=."
        cmd $ "tar -xvf " ++ target ++ ".tar.gz"

      rmFiles :: [FilePath] -> IO ()
      rmFiles fs = forM_ fs $
        \f -> removeFile f `catchIOError` (const  $ return ())

      rmDirs :: [FilePath] -> IO ()
      rmDirs ds = forM_ ds $
        \d -> removeDirectoryRecursive d `catchIOError` (const $ return ())
