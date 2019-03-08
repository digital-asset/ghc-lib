
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
    let cmd x = do
            putStrLn $ "\n\n# Running: " ++ x
            hFlush stdout
            (t, _) <- duration $ system_ x
            putStrLn $ "# Completed in " ++ showDuration t ++ ": " ++ x ++ "\n"
            hFlush stdout

    when isWindows $
        cmd "stack exec -- pacman -S autoconf automake-wrapper make patch python tar --noconfirm"
    -- We want to use gitlab but Travis has issues:
    -- "fatal: unable to access 'https://gitlab.haskell.org/ghc/ghc.git/': gnutls_handshake() failed: Handshake failed"
    -- cmd "git clone https://gitlab.haskell.org/ghc/ghc.git --recursive" -- --recurse-submodules=libraries/Cabal
    cmd $ unlines ["cat << EOF >> ~/.gitconfig"
                  ,"[url \"git://github.com/ghc/packages-\"]"
                  ,"insteadOf = git://github.com/ghc/packages/"
                  ,"[url \"git@github.com:ghc/packages-\"]"
                  ,"insteadOf = git@github.com:ghc/packages/"
                  ,"EOF"]
    cmd "git clone https://github.com/ghc/ghc.git --recursive"
    -- not essential, but make the cache work between Hadrian and ghc-lib and build Hadrian quicker
    appendFile "ghc/hadrian/stack.yaml" $ unlines ["ghc-options:","  \"$everything\": -O0 -j"]
    cmd "stack exec -- ghc-lib-gen ghc"

    -- Generate an sdist and extract it to ensure it works
    stackYaml <- readFile' "stack.yaml"
    writeFile "stack.yaml" $ stackYaml ++ unlines ["- ghc"]
    cmd "stack sdist ghc --tar-dir=."
    [tarball] <- filter (isExtensionOf ".tar.gz") <$> getDirectoryContents "."
    cmd $ "tar -xf " ++ tarball
    renameDirectory (dropExtension $ dropExtension tarball) "ghc-lib"

    -- Test the new projects
    writeFile "stack.yaml" $
      stackYaml ++
      unlines [ "- ghc-lib"
              , "- examples/mini-hlint"
              , "- examples/mini-compile"
              ]
    cmd "stack build --no-terminal --interleaved-output"
    cmd "stack exec --no-terminal -- ghc-lib --version"
    cmd "stack exec --no-terminal -- mini-hlint examples/mini-hlint/test/MiniHlintTest.hs"
    cmd "stack exec --no-terminal -- mini-compile examples/mini-compile/test/MiniCompileTest.hs"
