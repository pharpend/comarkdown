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
-- Module      : ExmaplesSpec
-- Description : This runs all of the tests for the examples/ directory
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module ExamplesSpec where

import Text.Comarkdown

import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.List
import System.Directory
import Test.Hspec

spec :: Spec
spec =
  do testsPath <- runIO (makeAbsolute "tests/examples")
     dirContents <- runIO (getDirectoryContents testsPath)
     let testPaths = sort (filter (isSuffixOf ".in.comd") dirContents)
         expPaths = sort (filter (isSuffixOf ".out.md") dirContents)
     pure ()
     -- forM_ (zip testPaths expPaths) $
     --   \(tf,rf) -> 
     --     specify (mconcat ["Parsing ", tf, " matches ",  rf]) $ 
     --     do tfContents <- B.readFile (mconcat [testsPath, "/", tf])
     --        rfContents <- B.readFile (mconcat [testsPath, "/", rf])
     --        parseResult <- comdParse tf tfContents
     --        shouldBe (fmap runDocument parseResult) 
     --                 (Right rfContents)
