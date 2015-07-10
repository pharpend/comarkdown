{-# LANGUAGE OverloadedLists #-}
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
-- Module      : CommandSpec
-- Description : Tests for parsing of \let macros.
-- Copyright   : Copyright 2015 Peter Harpending.
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module CommandSpec where

import qualified Data.Map as M
import Text.Comarkdown
import Test.Hspec

spec :: Spec
spec =
  context "\\let commands" $
  do context "Definitions" $ 
      do specify "\\let{foo}{bar,baz}{$bar + $baz}" $ 
           do result <- parse "test" "\\let{foo}{bar,baz}{$bar + $baz}"
              result `shouldBe`
                Right [Define (DefCommand "foo" 
                                          (M.fromList [("bar", Nothing)
                                                      ,("baz", Nothing)])
                                          "$bar + $baz")]
         specify "\\let{foo}{bar={baz}, bop={quuz}}{$bar + $baz}" $ 
           do result <- parse "test" "\\let{foo}{bar,baz}{$bar + $baz}"
              result `shouldBe`
                Right [Define (DefCommand "foo" 
                                          (M.fromList [("bar", Just "baz")
                                                      ,("bop", Just "quuz")])
                                          "$bar + $baz")]

     context "Applications" $ return ()
