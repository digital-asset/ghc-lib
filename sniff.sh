#!/usr/bin/env bash

include_dirs="-I./ghc/ghc-lib/stage0/compiler/build \
-I./ghc/ghc-lib/stage1/compiler/build \
-I./ghc/compiler \
-I./ghc/compiler/utils \
-I./ghc/ghc-lib/generated"

hs_src_dirs="-ighc/ghc-lib/generated/ \
-ighc/compiler \
-ighc/compiler/backpack \
-ighc/compiler/basicTypes \
-ighc/compiler/cmm \
-ighc/compiler/codeGen \
-ighc/compiler/coreSyn \
-ighc/compiler/deSugar \
-ighc/compiler/ghci \
-ighc/compiler/hieFile \
-ighc/compiler/hsSyn \
-ighc/compiler/iface \
-ighc/compiler/llvmGen \
-ighc/compiler/main \
-ighc/compiler/nativeGen \
-ighc/compiler/parser \
-ighc/compiler/prelude \
-ighc/compiler/profiling \
-ighc/compiler/rename \
-ighc/compiler/simplCore \
-ighc/compiler/simplStg \
-ighc/compiler/specialise \
-ighc/compiler/stgSyn \
-ighc/compiler/stranal \
-ighc/compiler/typecheck \
-ighc/compiler/types \
-ighc/compiler/utils \
-ighc/ghc-lib/stage0/compiler/build \
-ighc/ghc-lib/stage1/compiler/build \
-ighc/libraries/ghc-boot \
-ighc/libraries/ghc-boot-th \
-ighc/libraries/ghc-heap \
-ighc/libraries/ghci \
-ighc/libraries/template-haskell"

parser_file="ghc/ghc-lib/stage0/compiler/build/Parser.hs"
packages="-ignore-package ghc" # "-package ghc -package base"

cmd="ghc -dep-suffix '' -dep-makefile .parser-depends \
-M $include_dirs $packages $hs_src_dirs $parser_file"
rm -f .parser-depends
eval "$cmd"

rm -f .parser-stripped
for i in `cat .parser-depends | grep "\.hs" | awk '{print $NF}'`; do
  basename $i | cut -f 1 -d '.' >> .parser-stripped
done
cat .parser-stripped | sort | uniq
