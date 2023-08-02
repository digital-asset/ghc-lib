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
               | Ghc941 | Ghc942 | Ghc943 | Ghc944 | Ghc945
               | Ghc961 | Ghc962
               | Ghc981
               | GhcMaster
  deriving (Show, Eq, Ord)

data GhcSeries = Ghc88 | Ghc810 | Ghc90 | Ghc92 | Ghc94 | Ghc96 | Ghc98 | Ghc910
  deriving (Eq, Ord)

instance Show GhcSeries where
  show = \case
    Ghc910  -> "ghc-9.10"
    Ghc98  -> "ghc-9.8"
    Ghc96  -> "ghc-9.6"
    Ghc94  -> "ghc-9.4"
    Ghc92  -> "ghc-9.2"
    Ghc90  -> "ghc-9.0"
    Ghc810 -> "ghc-8.10"
    Ghc88  -> "ghc-8.8"

ghcSeries :: GhcFlavor -> GhcSeries
ghcSeries = \case
    f | DaGhc881 <= f && f < Ghc8101 -> Ghc88
    f | Ghc8101 <= f && f < Ghc901 -> Ghc810
    f | Ghc901 <= f && f < Ghc921 -> Ghc90
    f | Ghc921 <= f && f < Ghc941 -> Ghc92
    f | Ghc941 <= f && f < Ghc961 -> Ghc94
    f | Ghc961 <= f && f < Ghc981 -> Ghc96
    f | Ghc981 <= f && f < GhcMaster -> Ghc98
    GhcMaster -> Ghc910
    _ -> error "ghcSeries: impossible case"
