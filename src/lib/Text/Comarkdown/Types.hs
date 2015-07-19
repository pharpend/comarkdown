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
-- Description : The types for the comarkdown library
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module Text.Comarkdown.Types where

import Data.Default
import Data.Text (Text)
import Data.Vector (Vector)

-- |The document state has a list of definitions
data DocumentState =
  DocumentState {definedCommands :: Vector Command
                ,definedEnvironments :: Vector Environment
                ,delimiters :: Delimiters}

-- |A command has a list of keywords, along with documentation.
data Command =
  Command   -- |This should not include the prefix (usually a backslash).
   {cmdPrimary :: CommandName
   ,
    -- |Ditto for these
    cmdAliases :: Vector CommandName
   ,cmdDoc :: DocString
   ,cmdFunction :: TextFunction}
   
-- |An environment is a bit more involved
data Environment =
  Environment {envPrimary :: EnvironmentName
              ,envAliases :: Vector EnvironmentName
              ,envDoc :: DocString
              ,
               -- |An environment *must* have some input. I.e. it has to do
               -- something with the stuff between @\begin{environment}@ and
               -- @\end{environment}@.
               --
               -- It can optionally require more arguments, but it must document
               -- them =p.
               envFunction :: Text -> TextFunction}

-- |Pretty self-explanatory
data Delimiters =
  Delimiters {commandPrefix :: Text
             ,lineCommentPrefix :: Text
             ,blockCommentPrefix :: Text
             ,blockCommentSuffix :: Text
             ,bracketStart :: Text
             ,bracketEnd :: Text
             ,bracketSep :: Text}

-- |> Delimiters "\\" "//" "/*" "*/" "{" "}" ","
instance Default Delimiters where
  def = Delimiters "\\" "//" "/*" "*/" "{" "}" ","

-- |A function which either produces a result or demands more input
data TextFunction
  = Result DocString Text
  | MoreInput DocString (Text -> TextFunction)

-- *** Semantic aliases for 'Text'
type DocString = Text
type CommandName = Text
type EnvironmentName = Text

class ToTextFunction a where
  toTextFunction :: a -> TextFunction

instance ToTextFunction TextFunction where
  toTextFunction = id

instance ToTextFunction (DocString, Text) where
  toTextFunction (d, t) = Result d t

instance ToTextFunction t => ToTextFunction (DocString, (Text -> t)) where
  toTextFunction (d, f) = 
    MoreInput d (toTextFunction . f)
