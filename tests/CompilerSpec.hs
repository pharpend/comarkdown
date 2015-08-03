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
-- Module      : CompilerSpec
-- Description : Specifications for the compiler
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module CompilerSpec where

import Control.Monad (forM_)
import Text.Comarkdown
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  parallel $
  describe "AST->Pandoc compiler" $
  do describe "Outputting plain-text" $
       do specify "given many Ignore blks, behave like text all in 1 ignore blk" $
            property $
            \(xs :: [String]) ->
              do result1 <- runDocument (mapM_ ignore xs)
                 result2 <- runDocument (ignore (mconcat xs))
                 shouldBe (writePlain def result1)
                          (writePlain def result2)
          specify "given many Comment blks, output is mempty" $
            property $
            \(xs :: [String]) ->
              do result <- runDocument (mapM_ comment xs)
                 shouldBe result mempty
          specify "given many Comment blks interspersed in ignore blocks, same result as ignore blks standalone" $
            property $
            \(xs :: [String],ys :: [String]) ->
              do shuffledList <-
                   generate (shuffle (mappend (fmap Ignore xs)
                                              (fmap Comment ys)))
                 result1 <- runDocument (mapM_ insertPart shuffledList)
                 result2 <-
                   runDocument
                     (forM_ shuffledList $
                      \case
                        Ignore s -> ignore s
                        _ -> return ())
                 shouldBe result1 result2
