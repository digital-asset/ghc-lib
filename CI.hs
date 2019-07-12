-- Copyright (c) 2019, Digital Asset (Switzerland) GmbH and/or its
-- affiliates. All rights reserved.  SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

-- CI script, compatible with all of Travis, Appveyor and Azure.

import Control.Monad
import System.Directory
import System.FilePath
import System.IO.Extra
import System.Info.Extra
import System.Process.Extra
import System.Time.Extra

main :: IO ()
main = do
    when isWindows $
        cmd "stack exec -- pacman -S autoconf automake-wrapper make patch python tar --noconfirm"

    cmd "git clone https://gitlab.haskell.org/ghc/ghc.git"
    cmd "cd ghc && git checkout de3935a6ccc26ec063e13d2739dd098c7616fde2" -- 07/12/2019
    cmd "cd ghc && git submodule update --init --recursive"

    appendFile "ghc/hadrian/stack.yaml" $ unlines ["ghc-options:","  \"$everything\": -O0 -j"]
    -- Build ghc-lib-gen. Do this here rather than in the Azure script
    -- so that it's not forgotten when testing this program locally.
    cmd "stack --no-terminal build"
    -- Make and extract an sdist of ghc-lib-parser.
    cmd "stack exec -- ghc-lib-gen ghc --ghc-lib-parser"
    tarball <- mkTarball
    renameDirectory (dropExtensions tarball) "ghc-lib-parser"
    removeFile tarball
    removeFile "ghc/ghc-lib-parser.cabal"
    cmd "git checkout stack.yaml"

    cmd "cd ghc && git checkout ."
    appendFile "ghc/hadrian/stack.yaml" $ unlines ["ghc-options:","  \"$everything\": -O0 -j"]

    -- Make and extract an sdist of ghc-lib.
    cmd "stack exec -- ghc-lib-gen ghc --ghc-lib"
    tarball <- mkTarball
    renameDirectory (dropExtensions tarball) "ghc-lib"
    removeFile tarball
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
              ]

    -- Separate the two library build commands so they are
    -- independently timed. Note that optimizations in these builds
    -- are disabled in stack.yaml via `ghc-options: -O0`.
    cmd "stack build ghc-lib-parser --no-terminal --interleaved-output"
    cmd "stack build ghc-lib --no-terminal --interleaved-output"
    cmd "stack build mini-hlint mini-compile --no-terminal --interleaved-output"
    -- Run tests.
    cmd "stack exec --no-terminal -- ghc-lib --version"
    cmd "stack exec --no-terminal -- mini-hlint examples/mini-hlint/test/MiniHlintTest.hs"
    cmd "stack exec --no-terminal -- mini-hlint examples/mini-hlint/test/MiniHlintTest_fatal_error.hs"
    cmd "stack exec --no-terminal -- mini-hlint examples/mini-hlint/test/MiniHlintTest_non_fatal_error.hs"
    cmd "stack exec --no-terminal -- mini-hlint examples/mini-hlint/test/MiniHlintTest_respect_dynamic_pragma.hs"
    cmd "stack exec --no-terminal -- mini-hlint examples/mini-hlint/test/MiniHlintTest_fail_unknown_pragma.hs"
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

      mkTarball :: IO String
      mkTarball = do
        stackYaml <- readFile' "stack.yaml"
        writeFile "stack.yaml" $ stackYaml ++ unlines ["- ghc"]
        sDistCreateExtract

      sDistCreateExtract :: IO String
      sDistCreateExtract = do
        cmd "stack sdist ghc --tar-dir=."
        [tarball] <- filter (isExtensionOf ".tar.gz") <$> getDirectoryContents "."
        cmd $ "tar -xf " ++ tarball
        return tarball
