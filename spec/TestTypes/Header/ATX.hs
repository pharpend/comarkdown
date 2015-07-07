{-# LANGUAGE OverloadedStrings #-}

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

-- |Helper types for ATX-style headers
module TestTypes.Header.ATX where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Test.QuickCheck
import TestTypes.Text ()

-- |'Text's without line breaks, or starting with '#'s
newtype ATX = ATX {unATX :: Text}
  deriving (Eq, Show)

instance Arbitrary ATX where
  arbitrary =
    fmap ATX $
    suchThat arbitrary $
    \x ->
      not (T.isInfixOf "\n" x || T.isPrefixOf "#" (T.strip x))

-- |A non-empty 'ATX'.
newtype ATXNonEmpty = ATXNE {unATXNE :: Text}
  deriving (Eq, Show)

instance Arbitrary ATXNonEmpty where
  arbitrary =
    fmap (ATXNE . unATX) $ suchThat arbitrary $ not . T.null . unATX
