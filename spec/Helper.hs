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

-- |Helper functions and types for testing
module Helper where

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Text.Comarkdown
import           Test.Hspec
import           Test.QuickCheck

-- |Test the result of parsing something
runParseTest :: String                 -- ^A string describing the test
             -> Text                   -- ^The string to parse
             -> Either String Document -- ^The result we're supposed to get
             -> SpecWith (Arg (IO ()))
runParseTest spec bs supposedResult = it spec $ do
  res <- parse "test input" bs
  res `shouldBe` supposedResult

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

instance Arbitrary Text where
  arbitrary = fmap T.pack arbitrary

-- |'Text's without sequential newlines or any '#'s
newtype ATX = ATX {unATX :: Text}
  deriving (Eq, Show)

instance Arbitrary ATX where
  arbitrary =
    fmap ATX $
    suchThat arbitrary $
    not . T.isInfixOf "\n\n"

-- |A non-empty 'ATX'.
newtype ATXNonEmpty =
  ATXNE {unATXNE :: Text}
  deriving (Eq,Show)

instance Arbitrary ATXNonEmpty where
  arbitrary = fmap (ATXNE . unATX) $
              suchThat arbitrary $
              not . T.null . unATX

-- |'Text's without sequential newlines or any lines beginning with a
-- =. Trailing newlines are also forbidden.
newtype Setext1 = Setext1 {unSetext1 :: Text}
  deriving (Eq, Show)

instance Arbitrary Setext1 where
  arbitrary =
    fmap Setext1 $
    suchThat arbitrary $
    \s ->
      not $
      (T.isInfixOf "\n\n" s ||
       any (T.isPrefixOf "=")
           (T.lines s) ||
       T.isSuffixOf "\n" s)

-- |A non-empty 'Setext1'.
newtype Setext1NonEmpty = Setext1NE {unSetext1NE :: Text}
  deriving (Eq,Show)

instance Arbitrary Setext1NonEmpty where
  arbitrary = fmap (Setext1NE . unSetext1) $
              suchThat arbitrary $
              not . T.null . unSetext1


-- |'Text's without sequential newlines or any lines starting with '-'. Trailing
-- newlines are also forbidden.
newtype Setext2 = Setext2 {unSetext2 :: Text}
  deriving (Eq, Show)

instance Arbitrary Setext2 where
  arbitrary =
    fmap Setext2 $
    suchThat arbitrary $
    \s ->
      not $
      (T.isInfixOf "\n\n" s ||
       any (T.isPrefixOf "-")
           (T.lines s) ||
       T.isSuffixOf "\n" s)

-- |A non-empty 'Setext2'.
newtype Setext2NonEmpty = Setext2NE {unSetext2NE :: Text}
  deriving (Eq,Show)

instance Arbitrary Setext2NonEmpty where
  arbitrary = fmap (Setext2NE . unSetext2) $
              suchThat arbitrary $
              not . T.null . unSetext2


-- |Horizontal space
newtype HSpace = HSpace {unHSpace :: Text}
  deriving (Eq, Show)

instance Arbitrary HSpace where
  arbitrary = fmap (HSpace . T.pack)
                   (listOf (elements " \t"))
