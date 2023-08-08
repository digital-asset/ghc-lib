-- Copyright (c) 2019 - 2023 Digital Asset (Switzerland) GmbH and/or
-- its affiliates. All rights reserved. SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

{-# LANGUAGE LambdaCase #-}

module GhclibgenFlavor(
    GhcFlavor(..), GhcSeries(..), ghcSeries
) where

data GhcFlavor = DaGhc881
               | Ghc881 | Ghc882 | Ghc883 | Ghc884
               | Ghc8101 | Ghc8102 | Ghc8103 | Ghc8104 | Ghc8105 | Ghc8106 | Ghc8107
               | Ghc901 | Ghc902
               | Ghc921 | Ghc922 | Ghc923 | Ghc924 | Ghc925 | Ghc926 | Ghc927 | Ghc928
               | Ghc941 | Ghc942 | Ghc943 | Ghc944 | Ghc945 | Ghc946
               | Ghc961 | Ghc962
               | Ghc981
               | GhcMaster
  deriving (Show, Eq, Ord)

data GhcSeries = GHC_8_8 | GHC_8_10 | GHC_9_0 | GHC_9_2 | GHC_9_4 | GHC_9_6 | GHC_9_8 | GHC_9_10
  deriving (Eq, Ord)

instance Show GhcSeries where
  show = \case
    GHC_9_10  -> "ghc-9.10"
    GHC_9_8  -> "ghc-9.8"
    GHC_9_6  -> "ghc-9.6"
    GHC_9_4  -> "ghc-9.4"
    GHC_9_2  -> "ghc-9.2"
    GHC_9_0  -> "ghc-9.0"
    GHC_8_10 -> "ghc-8.10"
    GHC_8_8  -> "ghc-8.8"

ghcSeries :: GhcFlavor -> GhcSeries
ghcSeries = \case
    f | DaGhc881 <= f && f < Ghc8101 -> GHC_8_8
    f | Ghc8101 <= f && f < Ghc901 -> GHC_8_10
    f | Ghc901 <= f && f < Ghc921 -> GHC_9_0
    f | Ghc921 <= f && f < Ghc941 -> GHC_9_2
    f | Ghc941 <= f && f < Ghc961 -> GHC_9_4
    f | Ghc961 <= f && f < Ghc981 -> GHC_9_6
    f | Ghc981 <= f && f < GhcMaster -> GHC_9_8
    GhcMaster -> GHC_9_10
    _ -> error "ghcSeries: impossible case"
