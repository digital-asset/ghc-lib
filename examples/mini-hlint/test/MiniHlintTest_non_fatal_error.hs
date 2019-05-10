-- Copyright (c) 2019, Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: (Apache-2.0 OR BSD-3-Clause)

module MiniHlintTest_non_fatal_error where

import Data.List qualified

-- test/MiniHlintTest_non_fatal_error.hs:6:18: error:
--     Found `qualified' in postpositive position.
--     To allow this, enable language extension 'ImportQualifiedPost'
