import System.Process.Extra

main :: IO ()
main = do
    system_ "git clone git://git.haskell.org/ghc.git --recursive" -- --recurse-submodules=libraries/Cabal"
    system_ "stack exec -- ghc-lib-gen ghc"

    -- stack init doesn't work because of https://github.com/commercialhaskell/stack/issues/4508
    appendFile "stack.yaml" $ unlines ["- ghc","- examples/mini-hlint"]
    system_ "stack build --no-terminal --interleaved-output"
    system_ "stack exec --no-terminal -- ghc-lib --version"
    system_ "stack exec --no-terminal -- mini-hlint examples/mini-hlint/test/MiniHlintTest.hs"
