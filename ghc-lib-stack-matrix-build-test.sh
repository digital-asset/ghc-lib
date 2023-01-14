#!/usr/bin/env bash

set -euo pipefail

flavors=("ghc-master" "ghc-9.6.1")
resolvers=("ghc-9.4.4" "ghc-9.2.5" "ghc-9.2.2")
for f in "${flavors[@]}"; do
    for r in "${resolvers[@]}"; do
        echo "-- "
        stack runhaskell --stack-yaml stack-exact.yaml --resolver "$r" --package extra --package optparse-applicative CI.hs -- --ghc-flavor "$f" --stack-yaml stack-exact.yaml --resolver "$r" --no-checkout
    done
done
