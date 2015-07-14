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
-- Module      : Types
-- Description : Helper newtypes for the test suite
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module Types where

import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import Data.Char (isAscii, isSpace)
import Data.List
import Test.QuickCheck

-- |A line of markdown text; basically no line breaks or backslashes.
newtype MarkdownLine = MarkdownLine { unMarkdownLine :: ByteString }
  deriving (Eq, Show)

instance Arbitrary MarkdownLine where
  arbitrary = fmap (MarkdownLine . T.encodeUtf8 . T.pack) 
                   (suchThat arbitrary (not . any (`elem` controlChars)))
    where
      controlChars :: [Char]
      controlChars = "\\\n\r\f"

-- |Horizontal space; spaces or tabs
newtype HSpace = HSpace { unHSpace :: ByteString }
  deriving (Eq, Show)

instance Arbitrary HSpace where
  arbitrary = fmap (HSpace . T.encodeUtf8 . T.pack) 
                   (suchThat arbitrary (all (`elem` " \t")))

-- |Horizontal space, nonempty variant
newtype HSpaceNonEmpty =
  HSpaceNonEmpty {unHSpaceNonEmpty :: ByteString}
  deriving (Eq,Show)

instance Arbitrary HSpaceNonEmpty where
  arbitrary = do
    fst' <- suchThat arbitrary isHspaceChar
    fmap (HSpaceNonEmpty . T.encodeUtf8 . T.pack . (fst' :)) (suchThat arbitrary (all isHspaceChar))

    where
      isHspaceChar = flip elem " \t"


-- |Any space, including form feeds and carriage returns
newtype Space = Space { unSpace :: ByteString }
  deriving (Eq, Show)

instance Arbitrary Space where
  arbitrary = fmap (Space . T.encodeUtf8 . T.pack) (suchThat arbitrary (all isSpace))

-- |Any space, nonempty variant
newtype SpaceNonEmpty =
  SpaceNonEmpty {unSpaceNonEmpty :: ByteString}
  deriving (Eq,Show)

instance Arbitrary SpaceNonEmpty where
  arbitrary = 
    do fst' <- suchThat arbitrary isSpace'
       fmap (SpaceNonEmpty . T.encodeUtf8 . T.pack . (fst' :))
            (suchThat arbitrary (all isSpace'))
    where isSpace' = flip elem " \t\r\n\f"

newtype NonBreakingAscii = NonBreakingAscii { unNonBreakingAscii :: ByteString }
  deriving (Eq, Show)

instance Arbitrary NonBreakingAscii where
  arbitrary = fmap (NonBreakingAscii . T.encodeUtf8 . T.pack) 
                   (suchThat arbitrary (all isNonBreakingAscii))
    where
      isNonBreakingAscii x = not (elem x "\r\n\f") && isAscii x

-- |Any space, including form feeds and carriage returns
newtype BlockComment = BlockComment { unBlockComment :: ByteString }
  deriving (Eq, Show)

instance Arbitrary BlockComment where
  arbitrary = fmap (BlockComment . T.encodeUtf8 . T.pack) (suchThat arbitrary isBlockComment)
    where
      isBlockComment x = all isAscii x && not (isInfixOf "*/" x)

-- |Any space, including form feeds and carriage returns
newtype Nat = Nat { unNat :: Int }
  deriving (Eq, Show)

instance Arbitrary Nat where
  arbitrary = fmap Nat (suchThat arbitrary (>= 0))
