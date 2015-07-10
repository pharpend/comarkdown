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

-- |Wrappers around 'Text'
module TestTypes.Text where

#if __GLASGOW_HASKELL < 710
import Control.Applicative
#endif
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Test.QuickCheck

instance Arbitrary Text where
  arbitrary = fmap T.pack arbitrary

-- |Horizontal space
newtype HSpace = HSpace {unHSpace :: Text}
  deriving (Eq, Show)

instance Arbitrary HSpace where
  arbitrary = fmap (HSpace . T.pack)
                   (listOf (elements " \t"))

-- |Vertical space
newtype VSpace = VSpace {unVSpace :: Text}
  deriving (Eq, Show)

instance Arbitrary VSpace where
  arbitrary = fmap (VSpace . T.pack)
                   (listOf (pure '\n'))

-- |Whitespace
newtype WhiteSpace = WhiteSpace {unWhiteSpace :: Text}
  deriving (Eq, Show)

instance Arbitrary WhiteSpace where
  arbitrary = fmap (WhiteSpace . T.pack)
                   (listOf (elements " \t\f\r\n"))

-- |Any 'Text' without newlines or carriage returns
newtype NonBreaking = NonBreaking {unNonBreaking :: Text}
  deriving (Eq, Show)

instance Arbitrary NonBreaking where
  arbitrary =
    fmap NonBreaking $
    suchThat arbitrary $
    \x -> not $ T.any (`elem` ("\r\n" :: String)) x

