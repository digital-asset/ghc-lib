import Control.Monad
import System.Info.Extra
import System.Process.Extra
import System.Time.Extra

main :: IO ()
main = do
    let cmd x = do
            putStrLn $ "\n\n# Running: " ++ x
            (t, _) <- duration $ system_ x
            putStrLn $ "# Completed in " ++ showDuration t ++ ": " ++ x ++ "\n"

    when isWindows $
        cmd "stack exec -- pacman -S autoconf automake-wrapper make patch python tar --noconfirm"
    cmd "git clone git://git.haskell.org/ghc.git --recursive" -- --recurse-submodules=libraries/Cabal"
    cmd "stack exec -- ghc-lib-gen ghc"

    -- Add the new projects to the stack.yaml, so we compile/test them
    appendFile "stack.yaml" $ unlines ["- ghc","- examples/mini-hlint"]
    cmd "stack build --no-terminal --interleaved-output"
    cmd "stack exec --no-terminal -- ghc-lib --version"
    cmd "stack exec --no-terminal -- mini-hlint examples/mini-hlint/test/MiniHlintTest.hs"
