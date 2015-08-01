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
-- Module      : ExamplesSpec
-- Description : Runs the examples.
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module ExamplesSpec where

import Control.Monad (forM_)
import Data.List (sort, isSuffixOf)
import System.Directory
import Test.Hspec
import Text.Comarkdown

spec :: Spec
spec =
  parallel $
  describe "Examples with an input & output file" $
  do dirPath <- runIO $ makeAbsolute "tests/io-examples/"
     dirContents <- runIO $ getDirectoryContents dirPath
     let dirContents' = drop 2 (sort dirContents)
         inputPaths = filter (isSuffixOf "in.md") dirContents'
         outputPaths = filter (isSuffixOf "out.md") dirContents'
     forM_ (zip inputPaths outputPaths) $
       \(ip,op) ->
         specify (mconcat ["Compiling ",ip," matches ",op]) $
         do inputFile <- makeAbsolute (mappend dirPath ip)
            outputFile <- makeAbsolute (mappend dirPath op)
            compiledInput <- comdToMd inputFile
            output <- readFile outputFile
            compiledInput `shouldBe` output
