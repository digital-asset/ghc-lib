# ghc-lib [![Linux Build Status](https://img.shields.io/travis/digital-asset/ghc-lib/master.svg?label=Linux%20%26%20Mac%20builds)](https://travis-ci.org/digital-asset/ghc-lib)  [![Windows Build Status](https://img.shields.io/appveyor/ci/shayne-fletcher-da/ghc-lib/master.svg?label=Windows%20build)](https://ci.appveyor.com/project/shayne-fletcher-da/ghc-lib)
Copyright Digital Asset 2018-2019.

The [GHC API](https://hackage.haskell.org/package/ghc) allows you use the [GHC compiler](https://www.haskell.org/ghc/) as a library, so you can parse, analyse and compile Haskell code. The GHC API comes pre-installed with GHC, and is tied to that GHC version - if you are using GHC 8.6.3, you get version 8.6.3 of the API, and can't change it. The `ghc-lib` package solves that problem, letting you mix and match versions of the GHC compiler and GHC API. Why might you want that?

* Imagine you are writing a tool to work with several versions of the GHC compiler. The GHC API changes significantly between each version, so doing this would require writing a lot of C preprocessor code to support it. An alternative is to use one version of `ghc-lib` which works across multiple versions of GHC.
* Imagine you are modifying the GHC API or want features from GHC HEAD. With `ghc-lib` you can depend on the revised GHC API, without upgrading the compiler used to build everything, speeding up iteration.

While `ghc-lib` provides the full GHC API, it doesn't contain a runtime system, nor does it create a package database. That means you can't run code produced by `ghc-lib` (no runtime), and compiling off-the-shelf code is very hard (no package database containing the `base` library). What you can do:

* Parse Haskell code, making `ghc-lib` a potential replacement for [`haskell-src-exts`](https://hackage.haskell.org/package/haskell-src-exts). See the demo [`mini-hlint`](https://github.com/digital-asset/ghc-lib/blob/master/examples/mini-hlint/src/Main.hs) in this repo;
* Compile Haskell code as far as GHC's [Core language](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType), which includes renaming and type checking. See the demo [`mini-compile`](https://github.com/digital-asset/ghc-lib/blob/master/examples/mini-compile/src/Main.hs) in this repo, and the carefully tailored [file it compiles](https://github.com/digital-asset/ghc-lib/blob/master/examples/mini-compile/test/MiniCompileTest.hs).

# Using `ghc-lib`

The package `ghc-lib` will be released on [Hackage](https://hackage.haskell.org/), and can be used like any normal package, e.g. `cabal install ghc-lib`. Since it conflicts perfectly with the GHC API, you may wish to enable the language extension `PackageImports` so that you can for example, `import "ghc-lib" GHC`.

# Creating `ghc-lib`

To build `ghc-lib` you'll need clones of this repository and the [GHC repository](https://git.haskell.org). In a bash shell, build with the following commands.

```bash
git clone git@github.com:digital-asset/ghc-lib.git
cd ghc-lib
git clone git://git.haskell.org/ghc.git --recursive
cabal run ghc
cd ghc
cabal install
```
*Warning : `ghc-lib` is known to work on all of MacOS, Linux and Windows. A distribution produced with `cabal sdist` on Linux/MacOS builds on Windows, but a `cabal sdist` produced on Windows does not build on MacOS/Linux.*
