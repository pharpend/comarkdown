{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or (at
-- your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- | 
-- Module      : Main
-- Description : The test suite for comarkdown
-- Copyright   : Copyright 2015 Peter Harpending.
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module Main where

import Text.Comarkdown
import Test.Hspec

main :: IO ()
main =
  hspec $ context "Parsing" $ context "Recognizing bare headers" $ header1Tests

header1Tests :: SpecWith ()
header1Tests =
  context "Header1" $
  do specify "'# something' should be an h1"
             (shouldBe (parseEither "# something")
                       (Right [(Markdown (Header1 "something"))]))
     specify "'something\\n====' should be an h1"
             (shouldBe (parseEither "something\n====")
                       (Right [(Markdown (Header1 "something"))]))
     specify "'something\\n====' should be an h1"
             (shouldBe (parseEither "something\n====")
                       (Right [(Markdown (Header1 "something"))]))
     specify "'something\\n=' should be an H1"
             (shouldBe (parseEither "something\n=")
                       (Right [(Markdown (Header1 "something"))]))
     specify "'# something\\n====' should be an h1 with the '='s as part of the header"
             (shouldBe (parseEither "# something\n====")
                       (Right [(Markdown (Header1 "something ===="))]))
     specify "'# something #' should be an h1"
             (shouldBe (parseEither "# something #")
                       (Right [(Markdown (Header1 "something"))]))
     specify "'# something ####' should be an h1"
             (shouldBe (parseEither "# something ####")
                       (Right [(Markdown (Header1 "something"))]))
     specify "'# something' with trailing spaces should be an h1"
             (shouldBe (parseEither "# something                       ")
                       (Right [(Markdown (Header1 "something"))]))
