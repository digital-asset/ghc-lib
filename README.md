# ghc-lib [![Linux Build Status](https://img.shields.io/travis/digital-asset/ghc-lib/master.svg?label=Linux%20%26%20Mac%20builds)](https://travis-ci.org/digital-asset/ghc-lib)  [![Windows Build Status](https://img.shields.io/appveyor/ci/shayne-fletcher-da/ghc-lib/master.svg?label=Windows%20build)](https://ci.appveyor.com/project/shayne-fletcher-da/ghc-lib)

`ghc-lib` is for embedding GHC, as a library in your own programs.

`ghc-lib` contains that part of a GHC source distribution implementing the compiler pipeline. The compiler version used to build `ghc-lib` may differ from the version of the sources. For example, you might build HEAD sources using version 8.6.3.

`ghc-lib` is useful when you require the services of a Haskell compiler at runtime. Some example applications:
  - quick feedback loops using `ghci` when hacking on GHC;
  - Haskell IDEs;
  - static analysis tools (like (HLint)[https://github.com/ndmitchell/hlint] for example).

# Building

To build `ghc-lib` you'll need clones of [this repository](git@github.com:digital-asset/ghc-lib.git) and a [GHC repository](git://git.haskell.org/ghc.git):
```
  cd <path-to-ghc-lib>/ghc-lib-gen
  cabal run <path-to-ghc>
  cd <path-to-ghc>
  cabal configure && cabal build && cabal install
```
where `<path-to-ghc-lib>` is the clone of the `ghc-lib` repository and `<path-to-ghc>` is the clone of the `ghc` repository.
