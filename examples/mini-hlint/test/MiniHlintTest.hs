-- Copyright 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All Rights Reserved.
-- SPDX-License-Identifier: (Apache-2.0 AND BSD-3-Clause)

module MiniHlintTest where

f :: Bool -> Int
f x = 1 + if id (not (not x)) || False then 1 else 2
