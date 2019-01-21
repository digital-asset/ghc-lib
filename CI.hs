import System.Process.Extra

main :: IO ()
main = do
    let cmd x = do putStrLn $ "\n\n# " ++ x; system_ x

    cmd "git clone git://git.haskell.org/ghc.git --recursive" -- --recurse-submodules=libraries/Cabal"
    cmd "stack exec -- ghc-lib-gen ghc"

    -- stack init doesn't work because of https://github.com/commercialhaskell/stack/issues/4508
    appendFile "stack.yaml" $ unlines ["- ghc","- examples/mini-hlint"]
    cmd "stack build --no-terminal --interleaved-output"
    cmd "stack exec --no-terminal -- ghc-lib --version"
    cmd "stack exec --no-terminal -- mini-hlint examples/mini-hlint/test/MiniHlintTest.hs"
