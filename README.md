# ghc-lib [![Linux Build Status](https://img.shields.io/travis/digital-asset/ghc-lib/master.svg?label=Linux%20%26%20Mac%20builds)](https://travis-ci.org/digital-asset/ghc-lib)  [![Windows Build Status](https://img.shields.io/appveyor/ci/shayne-fletcher-da/ghc-lib/master.svg?label=Windows%20build)](https://ci.appveyor.com/project/shayne-fletcher-da/ghc-lib)

`ghc-lib` is for embedding GHC as a library in your own programs.

`ghc-lib` contains that part of a GHC distribution implementing the compiler pipeline. The compiler version used for building may differ from the version of the GHC sources involved. For example, you might build HEAD sources using 8.6.3.

`ghc-lib` is useful when you require the services of a Haskell compiler at runtime. Some example applications are:
  - quick feedback loops using `ghci` when hacking on GHC itself;
  - Haskell IDEs;
  - static analysis tools (like [HLint](https://github.com/ndmitchell/hlint) for example).

# Build and install

To build and install `ghc-lib` you'll need clones of this repository and the [GHC repository](https://git.haskell.org/ghc.git)(with the git option `--recursive`). Then,
```
  cd <path-to-ghc-lib>/ghc-lib-gen
  cabal run <path-to-ghc>
  cd <path-to-ghc>
  cabal install
```
where `<path-to-ghc-lib>` and `<path-to-ghc>` are paths to the `ghc-lib` and `ghc` clones respectively.

*Note : Cabal distributions obtained via `cabal sdist` on Linux/MacOS are known to build on Windows but not vice-versa.*
