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
-- Copyright   : Copyright 2015 Peter Harpending.
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : portable
-- 
-- Comarkdown is a superset of markdown, with support for things like macros and
-- environments.
-- 
-- * How to use this library
-- 
-- > import Text.Comarkdown
-- 
-- * Pitfalls
-- 
-- This library uses the less popular
-- <https://hackage.haskell.org/package/parsec Parsec> library for parsing. Most
-- libraries nowadays use <https://hackage.haskell.org/package/attoparsec
-- attoparsec>.
-- 
-- So, why does this library use Parsec instead?
-- 
-- Essentially, there's a tradeoff: attoparsec is considerably faster, but at
-- the cost of error message quality. Parsec is slower, but it keeps track of
-- the state of the parser as it's going along, and therefore the error messages
-- are better. 
-- 
-- Since compiling Comarkdown _can_ have errors, it's important that the user
-- can comprehend the parser errors. Therefore, I went with Parsec instead of
-- attoparsec.

module Text.Comarkdown 
  ( -- ** Parsing
    Parser
  , DocumentState
  , parse
  , parse'
  , parseFile
  , parseFile'
  , comarkdownParser
    -- ** Types
  , Document
  , ComarkdownPart(..)
  , DefMacro(..)
  , ApplyMacro(..)
  , HeaderPart(..)
  , ParagraphPart(..)
  )
  where

import Text.Comarkdown.Parser
import Text.Comarkdown.Types
