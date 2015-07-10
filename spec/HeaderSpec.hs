{-# LANGUAGE CPP #-}
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

-- |Tests for parsing of Header{1..5} values
module HeaderSpec where

import TestTypes

#if __GLASGOW_HASKELL < 710
import Data.Monoid
#endif
import qualified Data.Text.Lazy as T
import Text.Comarkdown
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  parallel $
  do context "Recognizing bare headers" $
       do context "Markdown syntax" $
            do context "ATX (crunch) syntax" $
                 do atxHeader1
                    atxHeader2
                    atxHeader3
                    atxHeader4
                    atxHeader5
                    atxHeader6
               context "Setext (= and -) syntax" $
                 do setextHeader1
                    setextHeader2

-- |Parsing of bare 'Header1's using the following syntax
-- 
-- > # This is a header
atxHeader1 :: Spec
atxHeader1 =
  do context "Header1" $
       do specify (unwords ["A single '#'"
                           ,"++ arbitrary horizontal space"
                           ,"++ a non-empty ATX-compliant header"
                           ,"++ arbitrary horizontal space"
                           ,"++ zero or more '#'s"
                           ,"++ arbitrary horizontal space"
                           ,"should be an h1 containing the ATX-compliant header"]) $
            property $
            \(HSpace s,ATXHeaderTextNE h,HSpace t,Nat k,HSpace u) ->
              do let testInput =
                       mconcat ["#",s,h,t,T.pack (replicate k '#'),u]
                 _ <-
                   parse "test" testInput
                 pendingWith "Types have changed"
          specify "A sequence of lines all starting with a single '#' should be interpreted as one h1" $
            pending

-- |Parsing of bare 'Header2's using the following syntax
-- 
-- > ## This is a header
atxHeader2 :: Spec
atxHeader2 =
  context "Header2" $
  specify (unwords ["Two '#'s"
                   ,"++ arbitrary horizontal space"
                   ,"++ a non-empty ATX-compliant header"
                   ,"++ arbitrary horizontal space"
                   ,"++ an arbitrary number of '#'s"
                   ,"++ arbitrary horizontal space"
                   ,"should be an h2 containing the ATX-compliant header"]) $
  property $
  \(HSpace s,ATXHeaderTextNE h,HSpace t,Nat k,HSpace u) ->
    do let testInput = mconcat ["##",s,h,t,T.pack (replicate k '#'),u]
       _ <- parse "test" testInput
       pendingWith "Types have changed"

-- |Parsing of bare 'Header3's using the following syntax
-- 
-- > ### This is a header
atxHeader3 :: Spec
atxHeader3 =
  context "Header3" $
  specify (unwords ["Three '#'s"
                   ,"++ arbitrary horizontal space"
                   ,"++ a non-empty ATX-compliant header"
                   ,"++ arbitrary horizontal space"
                   ,"++ an arbitrary number of '#'s"
                   ,"++ arbitrary horizontal space"
                   ,"should be an h3 containing the ATX-compliant header"]) $
  property $
  \(HSpace s,ATXHeaderTextNE h,HSpace t,Nat k,HSpace u) ->
    do let testInput = mconcat ["###",s,h,t,T.pack (replicate k '#'),u]
       _ <- parse "test" testInput
       pendingWith "Types have changed"

-- |Parsing of bare 'Header4's using the following syntax
-- 
-- > #### This is a header
atxHeader4 :: Spec
atxHeader4 = 
  context "Header4" $
  specify (unwords ["Four '#'s"
                   ,"++ arbitrary horizontal space"
                   ,"++ a non-empty ATX-compliant header"
                   ,"++ arbitrary horizontal space"
                   ,"++ an arbitrary number of '#'s"
                   ,"++ arbitrary horizontal space"
                   ,"should be an h4 containing the ATX-compliant header"]) $
  property $
  \(HSpace s,ATXHeaderTextNE h,HSpace t,Nat k,HSpace u) ->
    do let testInput = mconcat ["####",s,h,t,T.pack (replicate k '#'),u]
       _ <- parse "test" testInput
       pendingWith "Types have changed"

-- |Parsing of bare 'Header5's using the following syntax
-- 
-- > ##### This is a header
atxHeader5 :: Spec
atxHeader5 =
  context "Header5" $
  specify (unwords ["Five '#'s"
                   ,"++ arbitrary horizontal space"
                   ,"++ a non-empty ATX-compliant header"
                   ,"++ arbitrary horizontal space"
                   ,"++ an arbitrary number of '#'s"
                   ,"++ arbitrary horizontal space"
                   ,"should be an h5 containing the ATX-compliant header"]) $
  property $
  \(HSpace s,ATXHeaderTextNE h,HSpace t,Nat k,HSpace u) ->
    do let testInput = mconcat ["#####",s,h,t,T.pack (replicate k '#'),u]
       _ <- parse "test" testInput
       pendingWith "Types have changed"



-- |Parsing of bare 'Header6's using the following syntax
-- 
-- > ###### This is a header
atxHeader6 :: Spec
atxHeader6 =
  context "Header6" $
  specify (unwords ["Six '#'s"
                   ,"++ arbitrary horizontal space"
                   ,"++ a non-empty ATX-compliant header"
                   ,"++ arbitrary horizontal space"
                   ,"++ an arbitrary number of '#'s"
                   ,"++ arbitrary horizontal space"
                   ,"should be an h6 containing the ATX-compliant header"]) $
  property $
  \(HSpace s,ATXHeaderTextNE h,HSpace t,Nat k,HSpace u) ->
    do let testInput = mconcat ["######",s,h,t,T.pack (replicate k '#'),u]
       _ <- parse "test" testInput
       pendingWith "Types have changed"


-- |Parsing of bare 'Header1's using the following syntax:
-- 
-- > This is a header
-- > ===============
setextHeader1 :: Spec
setextHeader1 =
  context "Header1" $
  do specify "A Setext1-compliant header followed by a row of '='s should be an h1" $
       property $
       \(Setext1NE s,Peano i) ->
         do let testInput = mconcat [s,"\n",T.pack (replicate i '=')]
            _ <- parse "test" testInput
            pendingWith "Types have changed"
     specify "A Setext1-compliant header followed by a row of ('='s surrounded by arbitrary whitespace) should be an h1, even if there is whitespace surrounding the entire expression" $
       property $
       \(WhiteSpace w,Setext1NE s,WhiteSpace v,Peano i,WhiteSpace x) ->
         do let testInput = mconcat [w,s,"\n",v,T.pack (replicate i '='),x]
            _ <- parse "test" testInput
            pendingWith "Types have changed"
     specify "1 or more '='s ++ 1 or more non-breaking characters should be paragraph text" $
       property $
       \(Peano i,NonBreaking t) ->
         do let testInput =
                  mappend (T.pack (replicate i '=')) t
            _ <- parse "test" testInput
            pendingWith "Types have changed"
     specify "optional Setext1-compliant header ++ newline if nonempty header ++ 1 or more '='s ++ 1 or more non-breaking characters should be paragraph text" $
       property $
       \(Setext1 s,Peano i,NonBreaking t) ->
         do let testInput =
                  mconcat [if T.null s
                              then mempty
                              else mappend s "\n"
                          ,T.pack (replicate i '=')
                          ,t]
            _ <- parse "test" testInput
            pendingWith "Types have changed"

-- |Parsing of bare 'Header2's using the following syntax:
-- 
-- > This is a header
-- > ---------------
setextHeader2 :: Spec
setextHeader2 = 
  context "Header2" $
  do specify "A Setext2-compliant header followed by a row of '-'s should be an h2" $
       property $
       \(Setext2NE s,Peano i) ->
         do let testInput = mconcat [s, "\n", T.pack (replicate i '-')]
            _ <- parse "test" testInput
            pendingWith "Types have changed"
