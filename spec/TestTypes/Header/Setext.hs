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

-- |Helper types for testing Setext-style headers
module TestTypes.Header.Setext where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Test.QuickCheck
import TestTypes.Text ()

-- |'Text's without blank lines, or any lines containing only '='s and
-- whitespace.
newtype Setext1 = Setext1 {unSetext1 :: Text}
  deriving (Eq, Show)

instance Arbitrary Setext1 where
  arbitrary =
    fmap Setext1 $
    suchThat arbitrary $
    \s ->
      not $
      -- Note that this case also covers blank lines, because the line will
      -- remain unchanged if you drop all of the '='s
      any (T.null . T.dropWhile (== '=') . T.strip)
          (T.lines s)

-- |A non-empty 'Setext1'.
newtype Setext1NonEmpty = Setext1NE {unSetext1NE :: Text}
  deriving (Eq,Show)

instance Arbitrary Setext1NonEmpty where
  arbitrary = fmap (Setext1NE . unSetext1) $
              suchThat arbitrary $
              not . T.null . unSetext1


-- |'Text's without blank lines, or lines containing only '-'s and whitespace.
newtype Setext2 = Setext2 {unSetext2 :: Text}
  deriving (Eq, Show)

instance Arbitrary Setext2 where
  arbitrary =
    fmap Setext2 $
    suchThat arbitrary $
    \s ->
      not $
      -- Note that this case also covers blank lines, because the line will
      -- remain unchanged if you drop all of the '='s
      any (T.null . T.dropWhile (== '-') . T.strip)
          (T.lines s)

-- |A non-empty 'Setext2'.
newtype Setext2NonEmpty = Setext2NE {unSetext2NE :: Text}
  deriving (Eq, Show)

instance Arbitrary Setext2NonEmpty where
  arbitrary = fmap (Setext2NE . unSetext2) $
              suchThat arbitrary $
              not . T.null . unSetext2
