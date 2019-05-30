-- Copyright (c) 2019, Digital Asset (Switzerland) GmbH and/or its
-- affiliates. All rights reserved.  SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

-- Test 'parsePragmasIntoDynFlags'. This should parse.
{-# LANGUAGE ImportQualifiedPost #-}

module MiniHlintTest_respect_dynamic_pragma where

import Data.List qualified
