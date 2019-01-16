
import System.Directory
import System.Process.Extra


main :: IO ()
main = do
    system_ "git clone --recursive --depth=1 git://gitlab.haskell.org/ghc/ghc.git"
    withCurrentDirectory "ghc-lib-gen" $ system_ "cabal run ../ghc"
    withCurrentDirectory "ghc" $ system_ "cabal build lib:ghc-lib"
