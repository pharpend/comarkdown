{-# LANGUAGE CPP #-}
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

import TestTypes.Numeric
import TestTypes.Text

#if __GLASGOW_HASKELL < 710
import Data.Monoid
#endif
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Test.QuickCheck
import TestTypes.Text ()

-- |'Text's without line breaks, or starting with '#'s
newtype ATXHeaderText = ATXHeaderText {unATXHeaderText :: Text}
  deriving (Eq, Show)

instance Arbitrary ATXHeaderText where
  arbitrary =
    fmap ATXHeaderText $
    suchThat arbitrary $
    \x ->
      not (T.isInfixOf "\n" x || T.isPrefixOf "#" (T.strip x))

-- |A non-empty 'ATXHeaderText'.
newtype ATXHeaderTextNonEmpty = ATXHeaderTextNE {unATXHeaderTextNE :: Text}
  deriving (Eq, Show)

instance Arbitrary ATXHeaderTextNonEmpty where
  arbitrary =
    fmap (ATXHeaderTextNE . unATXHeaderText) $
    suchThat arbitrary $ 
    not . T.null . unATXHeaderText

-- |An ATX 'Header1'
newtype ATX1 = ATX1 {unATX1 :: Text}

instance Arbitrary ATX1 where
  arbitrary = fmap (ATX1 . T.intercalate "\n") 
                   (listOf singleLine)
    where singleLine = 
            do HSpace s <- arbitrary
               ATXHeaderText t <- arbitrary
               HSpace u <- arbitrary
               Peano i <- arbitrary
               HSpace v <- arbitrary
               return (mconcat [T.pack $ replicate 1  '#'
                               ,s
                               ,t
                               ,u
                               ,T.pack (replicate i '#')
                               ,v])

-- |An ATX 'Header2'
newtype ATX2 = ATX2 {unATX2 :: Text}

instance Arbitrary ATX2 where
  arbitrary = fmap (ATX2 . T.intercalate "\n") 
                   (listOf singleLine)
    where singleLine = 
            do HSpace s <- arbitrary
               ATXHeaderText t <- arbitrary
               HSpace u <- arbitrary
               Peano i <- arbitrary
               HSpace v <- arbitrary
               return (mconcat [T.pack $ replicate 2  '#'
                               ,s
                               ,t
                               ,u
                               ,T.pack (replicate i '#')
                               ,v])
-- |An ATX 'Header3'
newtype ATX3 = ATX3 {unATX3 :: Text}

instance Arbitrary ATX3 where
  arbitrary = fmap (ATX3 . T.intercalate "\n") 
                   (listOf singleLine)
    where singleLine = 
            do HSpace s <- arbitrary
               ATXHeaderText t <- arbitrary
               HSpace u <- arbitrary
               Peano i <- arbitrary
               HSpace v <- arbitrary
               return (mconcat [T.pack $ replicate 3  '#'
                               ,s
                               ,t
                               ,u
                               ,T.pack (replicate i '#')
                               ,v])

-- |An ATX 'Header4'
newtype ATX4 = ATX4 {unATX4 :: Text}

instance Arbitrary ATX4 where
  arbitrary = fmap (ATX4 . T.intercalate "\n") 
                   (listOf singleLine)
    where singleLine = 
            do HSpace s <- arbitrary
               ATXHeaderText t <- arbitrary
               HSpace u <- arbitrary
               Peano i <- arbitrary
               HSpace v <- arbitrary
               return (mconcat [T.pack $ replicate 4  '#'
                               ,s
                               ,t
                               ,u
                               ,T.pack (replicate i '#')
                               ,v])

-- |An ATX 'Header5'
newtype ATX5 = ATX5 {unATX5 :: Text}

instance Arbitrary ATX5 where
  arbitrary = fmap (ATX5 . T.intercalate "\n") 
                   (listOf singleLine)
    where singleLine = 
            do HSpace s <- arbitrary
               ATXHeaderText t <- arbitrary
               HSpace u <- arbitrary
               Peano i <- arbitrary
               HSpace v <- arbitrary
               return (mconcat [T.pack $ replicate 5  '#'
                               ,s
                               ,t
                               ,u
                               ,T.pack (replicate i '#')
                               ,v])

-- |An ATX 'Header6'
newtype ATX6 = ATX6 {unATX6 :: Text}

instance Arbitrary ATX6 where
  arbitrary = fmap (ATX6 . T.intercalate "\n") 
                   (listOf singleLine)
    where singleLine = 
            do HSpace s <- arbitrary
               ATXHeaderText t <- arbitrary
               HSpace u <- arbitrary
               Peano i <- arbitrary
               HSpace v <- arbitrary
               return (mconcat [T.pack $ replicate 6  '#'
                               ,s
                               ,t
                               ,u
                               ,T.pack (replicate i '#')
                               ,v])
