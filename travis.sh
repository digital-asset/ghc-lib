#!/bin/sh
# This script is invoked from my Travis-CI commands
# It bootstraps to grab the 'neil' tool and run 'neil test'
set -e # exit on errors
set -x # echo each line

retry(){
    ($@) && return
    sleep 15
    ($@) && return
    sleep 15
    $@
}

# Check with HLint
curl -sSL https://raw.github.com/ndmitchell/hlint/master/misc/travis.sh | sh -s ghc-lib-gen

if [ "$TRAVIS_OS_NAME" = "linux" ]; then
    retry sudo add-apt-repository -y ppa:hvr/ghc
    # Sometimes apt-get update fails silently, but then apt-get install fails loudly, so retry both
    update_install(){
        sudo apt-get  --allow-unauthenticated update && sudo apt-get --allow-unauthenticated install ghc-$GHCVER cabal-install-2.2 happy-1.19.4 alex-3.1.3
    }
    retry update_install
    export PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/2.2/bin:/opt/happy/1.19.4/bin:/opt/alex/3.1.3/bin:$PATH
    retry cabal update
else
    brew update
    brew install ghc cabal-install
    retry cabal update
    cabal install alex happy
fi
export PATH=$HOME/.cabal/bin:$PATH

ghc --version
cabal --version
happy --version
alex --version
haddock --version

runhaskell CI.hs
