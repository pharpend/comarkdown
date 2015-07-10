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

-- |Wrapper types and 'Arbitrary' instances around other numeric types
module TestTypes.Numeric where

import Test.QuickCheck

-- |Natural numbers, i.e. integers >= 0
newtype Nat = Nat {unNat :: Int}
  deriving (Eq, Show)

instance Arbitrary Nat where
  arbitrary = Nat <$> (suchThat arbitrary (>= 0))

-- |Peano numbers, i.e. integers > 0
newtype Peano = Peano {unPeano :: Int}
  deriving (Eq, Show)

instance Arbitrary Peano where
  arbitrary = Peano <$> (suchThat arbitrary (> 0))
