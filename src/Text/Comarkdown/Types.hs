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
-- Module      : Text.Comarkdown.Types
-- Description : The types for Comarkdown
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable
-- 
-- This module is not very interesting.

module Text.Comarkdown.Types where

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as T
import Data.Vector (Vector)
import qualified Data.Vector as V

-- |Take a 'Document', and write it out
runDocument :: Document -> ByteString
runDocument = mconcat . V.toList . fmap runComdPart

-- |Write out an individual Comarkdown part
runComdPart :: ComdPart -> ByteString
runComdPart =
  \case
    Comment _ -> mempty
    Ignore x -> T.encodeUtf8 x
    EmptyPart -> mempty

type Document = Vector ComdPart

-- |Type for the parser
data ComdPart
    -- |A comment. I'm saving the comments for now, because I might set up some
    -- sort of documentation in the future.
  = Comment Text
    -- |Stuff to ignore.
  | Ignore Text
    -- |This is the equivalent of mzero or empty or whatever.
  | EmptyPart
  deriving (Eq,Show)

