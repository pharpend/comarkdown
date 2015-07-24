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

module Text.Comarkdown.Types 
  ( -- ** Re-exports from pandoc.
    Pandoc
  , readMarkdown
  , PandocError
    -- * Comarkdown types
  , module Text.Comarkdown.Types
  )
  where

import Control.Exceptional
import Control.Monad.State
import Data.Default
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import Text.Pandoc (Pandoc, readMarkdown)
import Text.Pandoc.Error (PandocError(..))

type DocumentM x = StateT Document IO x

-- |A more efficient representation of 'Document'; it does not include
-- documentation, and thus is for use when compiling documents.
data CompilerForm =
  CompilerForm {cfCommands :: HashMap CommandName TextFunction
               ,cfEnvironments :: HashMap EnvironmentName (Text -> Exceptional TextFunction)
               ,cfDelimiters :: Delimiters
               ,cfParts :: Vector DocumentPart}

-- |The document has a list of definitions, as well as the document up to this
-- point.
data Document =
  Document {definedCommands :: Vector Command
           ,definedEnvironments :: Vector Environment
           ,delimiters :: Delimiters
           ,docParts :: Vector DocumentPart}

toCf :: Document -> CompilerForm
toCf doc =
  CompilerForm {cfCommands =
                  foldMap (\cmd ->
                             H.singleton (cmdPrimary cmd)
                                         (cmdFunction cmd))
                          (definedCommands doc)
               ,cfEnvironments =
                  foldMap (\env ->
                             H.singleton (envPrimary env)
                                         (envFunction env))
                          (definedEnvironments doc)
               ,cfDelimiters = delimiters doc
               ,cfParts = docParts doc}

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
               envFunction :: Text -> Exceptional TextFunction}

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

-- |The main type for the parser.
data DocumentPart
  = Comment Text
  | Ignore Text
  | CommandCall CommandName (Vector Text)
  | EnvironmentCall EnvironmentName Text (Vector Text)
  deriving (Eq, Show)

-- |A function which either produces a result or demands more input
data TextFunction
  = Result DocString (Either PandocError Pandoc)
  | MoreInput DocString (Text -> Exceptional TextFunction)

-- *** Semantic aliases for 'Text'
type DocString = Text
type CommandName = Text
type EnvironmentName = Text
type MarkdownText = Text

-- |Yay overloading!
class ToTextFunction a where
  toTextFunction :: a -> TextFunction

instance ToTextFunction TextFunction where
  toTextFunction = id

instance ToTextFunction (DocString, Pandoc) where
  toTextFunction (d, f) = Result d (Right f)

instance ToTextFunction (DocString, PandocError) where
  toTextFunction (d, f) = Result d (Left f)

-- |Interpret resulting 'String' as markdown
instance ToTextFunction (DocString, String) where
  toTextFunction (d, f) = Result d (readMarkdown def f)

-- |Wrapper around instance with 'String's
instance ToTextFunction (DocString, Text) where
  toTextFunction (d, f) = Result d (readMarkdown def (T.unpack f))

instance ToTextFunction t => ToTextFunction (DocString, (Text -> Exceptional t)) where
  toTextFunction (d, f) = 
    MoreInput d (fmap toTextFunction . f)

-- |Wrapper around 'Text' instance
instance ToTextFunction t => ToTextFunction (DocString, (String -> Exceptional t)) where
  toTextFunction (d, f) = 
    MoreInput d (fmap toTextFunction . f . T.unpack)


