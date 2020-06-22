-- Copyright (c) 2019, Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: (Apache-2.0 OR BSD-3-Clause)

-- Based on https://github.com/ghc/ghc/blob/23f6f31dd66d7c370cb8beec3f1d96a0cb577393/libraries/ghc-prim/GHC/Types.hs

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}

module GHC.Types (
        -- Data types that are built-in syntax
        -- They are defined here, but not explicitly exported
        --
        --    Lists:          []( [], (::) )

        Bool(..), Int (..), Word, TextLit,
        Ordering(..),
        Symbol,
        ifThenElse,
        Multiplicity(..)
    ) where

import GHC.Prim

infixr 5 :

-- | The kind of constraints, like `Show a`
data Constraint

data Multiplicity = Many | One

-- | (Kind) This is the kind of type-level symbols.
-- Declared here because class IP needs it
data Symbol

-- | Documentation for lists
data [] a = [] | a : [a]


-- | Information about ordering
data Ordering = LT | EQ | GT

-- | A 64-bit integer.
data Int =
  I# Int#

-- This is a dummy type we need for string literals.
data Char

type TextLit = [Char]

-- A dummy type for Word.
data Word

data Bool = False | True

isTrue# :: Int# -> Bool
{-# INLINE isTrue# #-}
isTrue# x = tagToEnum# x

ifThenElse :: Bool -> a -> a -> a
ifThenElse c t f = case c of True -> t; False -> f

data Module = Module
                TrName   -- Package name
                TrName   -- Module name

data TrName
  = TrNameS Addr#  -- Static
  | TrNameD [Char] -- Dynamic

type KindBndr = Int

data RuntimeRep

data KindRep = KindRepTyConApp TyCon [KindRep]
             | KindRepVar !KindBndr
             | KindRepApp KindRep KindRep
             | KindRepFun KindRep KindRep
             | KindRepTYPE !RuntimeRep
             | KindRepTypeLitS TypeLitSort Addr#
             | KindRepTypeLitD TypeLitSort [Char]

data TypeLitSort = TypeLitSymbol
                 | TypeLitNat

data TyCon = TyCon Word# Word#           -- Fingerprint
                   Module                -- Module in which this is defined
                   TrName                -- Type constructor name
                   Int#                  -- How many kind variables do we accept?
                   KindRep               -- A representation of the type's kind
