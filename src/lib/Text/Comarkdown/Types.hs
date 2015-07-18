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
-- Description : The entire comarkdown library in one module
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module Text.Comarkdown.Types where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)

-- |The document state has a list of definitions
data DocumentState =
  DocumentState {definedCommands :: Vector Command
                ,definedEnvironments :: Vector Environment
                ,prefix :: Text}

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

-- |A function which either produces a result or demands more input
data TextFunction
  = Result DocString Text
  | MoreInput DocString (Text -> TextFunction)

-- *** Semantic aliases for 'Text'
type DocString = Text
type CommandName = Text
type EnvironmentName = Text

class ToTextFunction a where
  toTextFunction :: DocString -> a -> TextFunction

instance ToTextFunction Text where
  toTextFunction = Result

instance ToTextFunction String where
  toTextFunction d = Result d . T.pack

instance ToTextFunction t => ToTextFunction (Text -> t) where
  toTextFunction d f = 
    MoreInput d (\x -> toTextFunction d (f x))

instance ToTextFunction t => ToTextFunction (String -> t) where
  toTextFunction d f =
    MoreInput d
              (\x ->
                 toTextFunction d
                                (f (T.unpack x)))
