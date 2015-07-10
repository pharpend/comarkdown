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
-- Module      : Text.Comarkdown.Types
-- Description : The types for Comarkdown
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable
-- 
-- This module is not very interesting.

module Text.Comarkdown.Types where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text (Text)
import Data.Vector (Vector)

type Document = Vector ComdPart

-- |Type for the parser
data ComdPart
    -- |The definition of a command
  = DefCommand VarName
               VarMap
               Body
    -- |The application of a command
  | ApCommand VarName
              ApMap
    -- |A comment. I'm saving the comments for now, because I might set up some
    -- sort of documentation in the future.
  | Comment Text
    -- |Stuff to ignore.
  | Ignore ByteString
  deriving (Eq,Show)

-- |This one maybe needs a bit of explaining. 
-- 
-- When defining a command, you must give a list of arguments. With that, you
-- can optionally give a default value to each one.
type VarMap = Map VarName (Maybe Text)
type VarName = Text

-- |This is the type for a list of arguments to be sent to an 'ApCommand'.
type ApMap = Vector ApKV

-- |An argument can either be positional, or it can be a kwarg (i.e. a keyword
-- argument).
data ApKV
  = Positional VarName
  | Kwarg VarName Text
  deriving (Eq,Show)

-- |The body of commands
type Body = Vector BodyPart

-- |This can either be a bunch of stuff that we ignore, or "insert the
-- argument with key".
data BodyPart
  = Insert ByteString
  | ArgVal VarName
  deriving (Eq,Show)
