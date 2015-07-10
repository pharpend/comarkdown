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
-- Description : The Comarkdown library
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable
-- 
-- This is an umbrella module, which means that you only have to import this
-- module to access the entire Comarkdown API.
-- 
-- The documentation so far is abhorrent. I will add it when I actually have a
-- library to show.

module Text.Comarkdown 
  ( -- * Parsing
    Parser
  , parse
  , comdParser
    -- ** Running the results
  , runDocument
    -- * Types
  , Document
  , ComdPart(..)
  , VarMap
  , VarName
  , ApMap
  , ApKV(..)
  , Body
  , BodyPart(..))
  where


import Text.Comarkdown.Parser
import Text.Comarkdown.Types
