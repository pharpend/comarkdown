-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
-- 
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
-- details.
-- 
-- You should have received a copy of the GNU General Public License along with
-- this program.  If not, see <http://www.gnu.org/licenses/>.

-- | 
-- Module      : Text.Comarkdown
-- Description : The entire comarkdown library in one module
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module Text.Comarkdown 
  ( module Control.Exceptional
  , module Text.Pandoc
  , module Text.Parsec
  , module Text.Comarkdown.Combinators
  , module Text.Comarkdown.Parser
  , module Text.Comarkdown.Types
  ) where

import Text.Comarkdown.Combinators
import Text.Comarkdown.Parser
import Text.Comarkdown.Types

import Control.Exceptional
import Text.Pandoc
import Text.Parsec hiding (parse)
