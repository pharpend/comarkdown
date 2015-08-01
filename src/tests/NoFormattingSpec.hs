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
-- Module      : NoFormattingSpec
-- Description : Tests for pure markdown (i.e. with no comarkdown formatting).
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module NoFormattingSpec where

import Paths_comarkdown
import Test.Hspec
import Text.Comarkdown

spec :: Spec
spec =
  parallel $
  describe "Parsing plain (i.e. no-comarkdown) documents" $
  describe "If a document has no comarkdown-specific formatting, running it through the comarkdown preprocessor should not change it" $
  do it "holds with the README" $
       do readmePath <- getDataFileName "README.md"
          comdResult <- runComd readmePath
          pdResult <- runPd readmePath
          comdResult `shouldBe` pdResult
  where runComd :: FilePath -> IO String
        runComd fp =
          do pandoc <- runDocument $ parseFile fp
             return (writePlain def pandoc)
        runPd :: FilePath -> IO String
        runPd fp =
          do fileContents <- readFile fp
             let Right pandoc = readMarkdown def fileContents
             return (writePlain def pandoc)
