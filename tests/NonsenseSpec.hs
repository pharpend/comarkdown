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

-- | 
-- Module      : NonsenseSpec
-- Description : Tests for the 'nonsense' parser
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module NonsenseSpec where

import Text.Comarkdown
import Types

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text.Lazy.Encoding as T
import Test.Hspec
import Test.QuickCheck
import Text.Parsec

spec :: Spec
spec =
  parallel $
  do test_whiteSpace
     test_lineComment
     test_blockComment
     test_nonsense
     test_nonsense_combos

test_whiteSpace :: Spec
test_whiteSpace =
  describe "whiteSpace parser" $
  do it "should interpret arbitrary horizontal space as EmptyPart" $
       property $
       \(HSpaceNonEmpty h) ->
         do result <-
              runParserT whiteSpace OK "test" h
            shouldBe result (Right EmptyPart)
     it "should interpret any arbitrary space as EmptyPart" $
       property $
       \(SpaceNonEmpty h) ->
         do result <-
              runParserT whiteSpace OK "test" h
            shouldBe result (Right EmptyPart)


test_lineComment :: Spec
test_lineComment =
  describe "lineComment parser" $
  do it "should interpret two slashes followed by an arbitrary nonbreaking string as a comment" $
       property $
       \(NonBreakingAscii s) ->
         do let testInput = mappend "//" s
            result <-
              runParserT lineComment OK "test" testInput
            shouldBe result (Right (Comment (T.decodeUtf8 s)))
     describe "many lineComment" $
       it "should interpret a number of line comments separately" $
       property $
       \lines' ->
         do let testInput =
                  B.unlines (fmap (mappend "//" . unNonBreakingAscii) lines')
                textLines =
                  fmap (T.decodeUtf8 . unNonBreakingAscii) lines'
            result <-
              runParserT (many lineComment)
                         OK
                         "test"
                         testInput
            shouldBe result (Right (fmap Comment textLines))

test_blockComment :: Spec
test_blockComment =
  describe "blockComment parser" $
  do it "should interpret /* followed by arbitrary string followed by */ as comment" $
       property $
       \(BlockComment s) ->
         do let testInput = mconcat ["/*",s,"*/"]
            result <-
              runParserT blockComment OK "test" testInput
            shouldBe result (Right (Comment (T.decodeUtf8 s)))
     describe "many blockComment" $
       it "should interpret a number of block comments separately" $
       property $
       \blocks' ->
         do let testInput =
                  mconcat (fmap (\x ->
                                   mconcat ["/*",unBlockComment x,"*/"])
                                blocks')
                textBlocks =
                  fmap (T.decodeUtf8 . unBlockComment) blocks'
            result <-
              runParserT (many blockComment)
                         OK
                         "test"
                         testInput
            shouldBe result (Right (fmap Comment textBlocks))

test_nonsense :: Spec
test_nonsense =
  context "nonsense parser" $
  do specify "should interpret arbitrary horizontal space as EmptyPart" $
       property $
       \(HSpaceNonEmpty h) ->
         do result <- runParserT nonsense OK "test" h
            shouldBe result (Right EmptyPart)
     specify "should interpret any arbitrary space as EmptyPart" $
       property $
       \(SpaceNonEmpty h) ->
         do result <- runParserT nonsense OK "test" h
            shouldBe result (Right EmptyPart)
     specify "should interpret two slashes followed by an arbitrary nonbreaking string as a comment" $
       property $
       \(NonBreakingAscii s) ->
         do let testInput = mappend "//" s
            result <- runParserT nonsense OK "test" testInput
            shouldBe result (Right (Comment (T.decodeUtf8 s)))
     specify "should interpret /* followed by arbitrary string followed by */ as comment" $
       property $
       \(BlockComment s) ->
         do let testInput = mconcat ["/*",s,"*/"]
            result <- runParserT nonsense OK "test" testInput
            shouldBe result (Right (Comment (T.decodeUtf8 s)))
     context "many nonsense" $
       do specify "should interpret a number of line comments separately" $
            property $
            \lines' ->
              do let testInput = B.unlines (fmap (mappend "//" . 
                                                  unNonBreakingAscii) 
                                                 lines')
                     textLines = fmap (T.decodeUtf8 . 
                                       unNonBreakingAscii) 
                                      lines'
                 result <- runParserT (many nonsense)
                                      OK
                                      "test"
                                      testInput
                 shouldBe result 
                          (Right (fmap Comment textLines))
          specify "should interpret a number of block comments separately" $
            property $
            \blocks' ->
              do let testInput = mconcat (fmap (mappend "/*" . 
                                                flip mappend "*/" . 
                                                unBlockComment)
                                               blocks')
                     textBlocks = fmap (T.decodeUtf8 . 
                                        unBlockComment) 
                                       blocks'
                 result <- runParserT (many nonsense)
                                      OK
                                      "test"
                                      testInput
                 shouldBe result 
                          (Right (fmap Comment textBlocks))

test_nonsense_combos :: Spec
test_nonsense_combos =
  do context "Generation of singular arbitrary nonsense" $ 
       do it "should parse arbitrary nonsense as exactly that" $ 
            property $
            \x -> do result <- runParserT nonsense OK "test" (inputify x)
                     shouldBe result (nonsense2ComdPart x)
          specify "many nonsense should parse singular arbitrary nonsense as a singular list item" $
            property $
            \x -> do result <- runParserT (many nonsense) OK "test" (inputify x)
                     shouldBe result (sequence [nonsense2ComdPart x])
     context "Generation of a list of arbitrary nonsenses" $
       it "should parse a bunch of nonsense in that order (eliminating duplicate whitespace though)." $
       property $
       \x -> do result <- runParserT (many nonsense) 
                                     OK 
                                     "test" 
                                     (B.intercalate "\n" (map inputify x))
                -- The foldParts is here because there is weird (expected)
                -- behaviour when two WhiteSpace things are sent next to each
                -- other.
                -- 
                -- Namely, the parser interprets all of the whitespace in one
                -- block, as it should, however, the random generation will
                -- think of them separately, and thus will have consecutive
                -- 'EmptyPart's in the expected result.
                -- 
                -- I'm too lazy to figure out how to remove consecutive
                -- 'EmptyPart's from the expected result, so I just removed all
                -- of them from both the result and the expected result.
                shouldBe (fmap foldParts result) 
                         (fmap foldParts (traverse nonsense2ComdPart x))
  where nonsense2ComdPart :: Nonsense -> Either ParseError ComdPart
        nonsense2ComdPart =
          Right .
          \case
            WhiteSpace _ -> EmptyPart
            LC (NonBreakingAscii s) -> Comment (T.decodeUtf8 s)
            BC (BlockComment s) -> Comment (T.decodeUtf8 s)
        inputify =
          \case
            WhiteSpace (SpaceNonEmpty s) -> s
            LC (NonBreakingAscii s) -> mappend "//" s
            BC (BlockComment s) -> mconcat ["/*",s,"*/"]
        foldParts = filter nonEmpty
        nonEmpty = 
          \case
            EmptyPart -> False
            _ -> True
