
import System.Directory
import System.Process.Extra


main :: IO ()
main = do
    system_ "curl https://gitlab.haskell.org/ghc/ghc/-/archive/master/ghc-master.tar --output ghc-master.tar"
    system_ "tar -xf ghc-master.tar"

    system_ "git clone git://git.haskell.org/ghc.git --recursive"
    system_ "curl -sSL https://get.haskellstack.org/ | sh"
    withCurrentDirectory "ghc-lib-gen" $ system_ "cabal run ../ghc"
    withCurrentDirectory "ghc" $ system_ "cabal build lib:ghc-lib"
