# ghc-lib [![Hackage version](https://img.shields.io/hackage/v/ghc-lib.svg?label=Hackage)](https://hackage.haskell.org/package/ghc-lib) [![Stackage version](https://www.stackage.org/package/ghc-lib/badge/nightly?label=Stackage)](https://www.stackage.org/package/ghc-lib) [![Build Status](https://dev.azure.com/digitalasset/ghc-lib/_apis/build/status/digital-asset.ghc-lib?branchName=master)](https://dev.azure.com/digitalasset/ghc-lib/_build/latest?definitionId=11&branchName=master)
Copyright © 2019, Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
SPDX-License-Identifier: (Apache-2.0 OR BSD-3-Clause)

The [GHC API](https://hackage.haskell.org/package/ghc) allows you to use the [GHC compiler](https://www.haskell.org/ghc/) as a library, so you can parse, analyze and compile Haskell code. The GHC API comes preinstalled with GHC, and is tied to that GHC version - if you are using GHC 8.6.3, you get version 8.6.3 of the API, and can't change it. The `ghc-lib` project solves that problem, letting you mix and match versions of the GHC compiler and GHC API. Why might you want that?

* Imagine you are writing a tool to work with several versions of the GHC compiler. The GHC API changes significantly between each version, so doing this would require writing a lot of C preprocessor code to support it. An alternative is to use one version of `ghc-lib` which works across multiple versions of GHC.
* Imagine you are modifying the GHC API or want features from GHC HEAD. With `ghc-lib` you can depend on the revised GHC API, without upgrading the compiler used to build everything, speeding up iteration.

The `ghc-lib` project provides two packages : `ghc-lib-parser` and `ghc-lib`. The `ghc-lib-parser` package is  that subset of the GHC API that is just enough to parse Haskell code. The `ghc-lib` package extends (and re-exports) `ghc-lib-parser` with the rest. While `ghc-lib` provides the full GHC API, it doesn't contain a runtime system, nor does it create a package database. That means you can't run code produced by `ghc-lib` (no runtime), and compiling off-the-shelf code is very hard (no package database containing the `base` library). What you can do:

* Parse Haskell code, making `ghc-lib-parser` a potential replacement for [`haskell-src-exts`](https://hackage.haskell.org/package/haskell-src-exts). See the demo [`mini-hlint`](https://github.com/digital-asset/ghc-lib/blob/master/examples/mini-hlint/src/Main.hs) in this repo;
* Compile Haskell code as far as GHC's [Core language](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType), which includes renaming and type checking. See the demo [`mini-compile`](https://github.com/digital-asset/ghc-lib/blob/master/examples/mini-compile/src/Main.hs) in this repo, and the carefully tailored [file it compiles](https://github.com/digital-asset/ghc-lib/blob/master/examples/mini-compile/test/MiniCompileTest.hs).

There are some downsides to `ghc-lib`:

* The lack of runtime means you can't run code, which includes running code at compile time, e.g. `TemplateHaskell`.
* While `ghc-lib` isn't tied to any specific GHC versions, it can only read package databases and `.hi` files for one particular version of GHC. That means your existing package database probably can't be consumed by `ghc-lib` (unless you happen to perfectly match the GHC version, in which case you could just have used the GHC API), and it doesn't ship with a package database so you'd have to painfully build your own.
* Compilation times for the `ghc-lib` packages are not small, taking approximately 5 minutes for each on our CI machines.

## Using `ghc-lib`

The packages `ghc-lib-parser` and `ghc-lib` are available on [Hackage](https://hackage.haskell.org/), and can be used like any normal packages, e.g. `cabal install ghc-lib`. Since `ghc-lib-parser` and `ghc-lib` conflict perfectly with the GHC API and [`template-haskell`](https://hackage.haskell.org/package/template-haskell), ~the packages are not exposed by default so if you use GHC directly, you need to pass `-package ghc-lib`, the [GHC user guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html#using-packages) has more information : use the language extension `PackageImports` to do `import "ghc-lib" ...` or `import "ghc-lib-parser" ...` as approriate. There are two release streams within the `ghc-lib` name:

* Version 8.8.1 will be the version of `ghc-lib` produced against the released GHC 8.8.1, once it comes out;
* Version [0.20190204](http://hackage.haskell.org/package/ghc-lib-0.20190204) is the version of `ghc-lib` using GHC HEAD on the date 2019-02-04.

The Hackage packages are licensed under the [BSD-3-Clause license](https://www.haskell.org/ghc/license.html), just like GHC itself. This repo, including the [examples](https://github.com/digital-asset/ghc-lib/tree/master/examples) and the [script that generates `ghc-lib`](https://github.com/digital-asset/ghc-lib/tree/master/ghc-lib-gen), are licensed under the [BSD-3-Clause OR Apache-2.0 license](https://github.com/digital-asset/ghc-lib/blob/master/LICENSE).

## Creating `ghc-lib`

We create the packages by taking a checkout of GHC, and combining the `ghc` package with the various dependencies it is tightly tied to (e.g. `template-haskell`) in two new cabal files `ghc-lib-parser.cabal` and `ghc-lib.cabal`. These new packages depend on a few generated outputs (which we build using the GHC build system) and some [Cmm files](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Cmm) (which we hack around by editing the Haskell source, because Cabal support for Cmm files is lacking). The [`ghc-lib-gen` directory](https://github.com/digital-asset/ghc-lib/tree/master/ghc-lib-gen) contains a script that puts all the pieces together. Because GHC itself is capable of being bootstrapped with older GHC versions (its [Stage0 build](https://ghc.haskell.org/trac/ghc/wiki/Building/Architecture/Idiom/Stages)), the generated `ghc-lib` also compiles with multiple GHC versions.

To build `ghc-lib-parser` and `ghc-lib` you need clones of this repository and the [GHC repository](https://git.haskell.org).

*Warning : `ghc-lib-parser` and `ghc-lib` are known to work on all of MacOS, Linux and Windows. Distributions produced with `cabal sdist` on Linux/MacOS build on Windows, but a `cabal sdist` produced on Windows does not build on MacOS/Linux.*

### Building `ghc-lib`

By far the easist way to produce `ghc-lib-parser` and `ghc-lib` packages is to execute the CI script which incidentally builds and executes the examples (this procedure makes versioned packages  based on the current date and expresses the version constraint between `ghc-lib` and `ghc-lib-parser` accordingly).

```bash
# Setup
git clone git@github.com:digital-asset/ghc-lib.git
cd ghc-lib
./CI.hs --ghc-flavor ghc-8.8.1
```

## Releasing `ghc-lib` (notes for maintainers)

Build `ghc-lib` using the [above instructions](#building-ghc-lib)  and upload the resulting `.tar.gz` files to [Hackage](https://hackage.haskell.org/upload).
