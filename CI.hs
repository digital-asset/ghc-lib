-- Copyright (c) 2019, Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: (Apache-2.0 OR BSD-3-Clause)

-- CI script, invoked by both Travis and Appveyor

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

    -- Make and extract an sdist of ghc-lib-parser.
    cmd "git clone https://gitlab.haskell.org/ghc/ghc.git --recursive"
    appendFile "ghc/hadrian/stack.yaml" $ unlines ["ghc-options:","  \"$everything\": -O0 -j"]
    cmd "stack exec -- ghc-lib-gen ghc --ghc-lib-parser"
    stackYaml <- readFile' "stack.yaml"
    writeFile "stack.yaml" $ stackYaml ++ unlines ["- ghc"]
    tarball <- sDistCreateExtract
    renameDirectory (dropExtensions tarball) "ghc-lib-parser"
    removeFile tarball
    removeFile "ghc/ghc-lib-parser.cabal"

    -- Make and extract an sdist of ghc-lib.
    cmd "cd ghc && git checkout ."
    appendFile "ghc/hadrian/stack.yaml" $ unlines ["ghc-options:","  \"$everything\": -O0 -j"]
    cmd "stack exec -- ghc-lib-gen ghc --ghc-lib"
    tarball <- sDistCreateExtract
    renameDirectory (dropExtensions tarball) "ghc-lib"
    removeFile tarball
    removeFile "ghc/ghc-lib.cabal"

    -- Test the new projects.
    writeFile "stack.yaml" $
      stackYaml ++
      unlines [ "- ghc-lib-parser"
              , "- ghc-lib"
              , "- examples/mini-hlint"
              , "- examples/mini-compile"
              ]
    cmd "stack build --no-terminal --interleaved-output"
    cmd "stack exec --no-terminal -- ghc-lib --version"
    cmd "stack exec --no-terminal -- mini-hlint examples/mini-hlint/test/MiniHlintTest.hs"
    cmd "stack exec --no-terminal -- mini-hlint examples/mini-hlint/test/MiniHlintTest_error_handling.hs"
    cmd "stack exec --no-terminal -- mini-compile examples/mini-compile/test/MiniCompileTest.hs"

    -- Test everything loads in GHCi, see https://github.com/digital-asset/ghc-lib/issues/27
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

      sDistCreateExtract :: IO String
      sDistCreateExtract = do
        cmd "stack sdist ghc --tar-dir=."
        [tarball] <- filter (isExtensionOf ".tar.gz") <$> getDirectoryContents "."
        cmd $ "tar -xf " ++ tarball
        return tarball
