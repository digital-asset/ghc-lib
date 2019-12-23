-- Copyright (c) 2019, Digital Asset (Switzerland) GmbH and/or its
-- affiliates. All rights reserved.  SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

{-# LANGUAGE LambdaCase #-}
module GhclibgenOpts(
    GhclibgenTarget(..)
  , GhclibgenOpts(..)
  , GhcFlavor(..)
  , ghclibgenVersion
  , ghclibgenOpts
) where

import Control.Applicative
import Options.Applicative
import Data.Maybe
import Data.Version (showVersion)
import Data.Semigroup ((<>))
import Paths_ghc_lib_gen (version)

-- | A ghc-lib-gen target.
data GhclibgenTarget = GhclibParser | Ghclib

-- | The type of ghc-lib-gen options.
data GhclibgenOpts = GhclibgenOpts {
    ghclibgenOpts_root :: !FilePath -- ^ Path to a GHC git repository.
  , ghclibgenOpts_target :: !GhclibgenTarget -- ^ What target?
  , ghclibgenOpts_ghcFlavor :: !GhcFlavor
 }

-- | A parser of the "--ghc-lib" target.
ghclib :: Parser GhclibgenTarget
ghclib = flag' Ghclib
  ( long "ghc-lib"
  <> help "Generate a ghc-lib.cabal"
  )

-- | A parser of the "--ghc-lib-parser" target.
ghclibParser :: Parser GhclibgenTarget
ghclibParser = flag' GhclibParser
  ( long "ghc-lib-parser"
  <> help "Generate a ghc-lib-parser.cabal"
  )

-- | A parser of "--version".
ghclibgenVersion :: Parser (a -> a)
ghclibgenVersion =
  infoOption
  (showVersion version)
  (long "version" <> help "Show version")

-- | A parser of a ghc-lib-gen target: `target := | "--ghc-lib-parser"
-- | "--ghc-lib" | /* nothing */`.
ghclibgenTarget :: Parser GhclibgenTarget
ghclibgenTarget =
  fromMaybe Ghclib Control.Applicative.<$> optional (ghclibParser <|> ghclib)

-- | A parser of ghc-lib-gen options: `opts := STRING target`.
ghclibgenOpts :: Parser GhclibgenOpts
ghclibgenOpts = GhclibgenOpts
  <$> argument str (metavar "GHC_ROOT")
  <*> ghclibgenTarget
  <*> ghcFlavorOpt

-- | We might want to factor this out so we can share it with CI.hs
-- but for now it doesn’t seem worth it and having CI.hs be
-- self-contained simplifies things.
data GhcFlavor = Ghc8101 | Ghc881 | DaGhc881 | GhcMaster
    deriving (Show, Eq)

ghcFlavorOpt :: Parser GhcFlavor
ghcFlavorOpt = option readFlavor
    ( long "ghc-flavor"
   <> help "The ghc-flavor to test against"
    )

readFlavor :: ReadM GhcFlavor
readFlavor = eitherReader $ \case
    "ghc-8.10.1" -> Right Ghc8101
    "ghc-8.8.1" -> Right Ghc881
    "da-ghc-8.8.1" -> Right DaGhc881
    "ghc-master" -> Right GhcMaster
    flavor -> Left $ "Failed to parse ghc flavor " <> show flavor <> " expected ghc-master, ghc-8.8.1, da-ghc-8.8.1 or ghc-8.10.1"
