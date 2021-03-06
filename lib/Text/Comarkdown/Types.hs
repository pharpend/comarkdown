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
import Data.Aeson
import Data.Default
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import Data.List (findIndex)
import Data.Traversable (for)
import Safe (headMay)
import Text.Pandoc (Pandoc, readMarkdown, ReaderOptions)
import Text.Pandoc.Error (PandocError(..))

type DocumentM x = StateT Document IO x

-- |A more efficient representation of 'Document'; it does not include
-- documentation, and thus is for use when compiling documents.
data CompilerForm =
  CompilerForm {cfCommands :: HashMap CommandName Command
               ,cfEnvironments :: HashMap EnvironmentName Environment
               ,cfDelimiters :: Delimiters
               ,cfParts :: [DocumentPart]
               ,cfUserState :: Value
               ,cfOptions :: ReaderOptions}

-- |The document has a list of definitions, as well as the document up to this
-- point.
data Document =
  Document {definedCommands :: [Command]
           ,definedEnvironments :: [Environment]
           ,delimiters :: Delimiters
           ,docParts :: [DocumentPart]
           ,docUserState :: Value
           ,docOptions :: ReaderOptions}

-- |A document containing no definitions or parts, with the default delimiters
nullDocument :: Document
nullDocument = Document mempty mempty def mempty Null def

toCf :: Document -> CompilerForm
toCf doc =
  CompilerForm {cfCommands =
                  foldMap (\cmd ->
                             mappend (H.singleton (cmdPrimary cmd)
                                                  cmd)
                                     (foldMap (\n -> H.singleton n cmd)
                                              (cmdAliases cmd)))
                          (definedCommands doc)
               ,cfEnvironments =
                  foldMap (\env ->
                             mappend (H.singleton (envPrimary env)
                                                  env)
                                     (foldMap (\n -> H.singleton n env)
                                              (envAliases env)))
                          (definedEnvironments doc)
               ,cfDelimiters = delimiters doc
               ,cfParts = docParts doc
               ,cfUserState = docUserState doc
               ,cfOptions = docOptions doc}

-- |A command has a list of keywords, along with documentation.
data Command =
  Command   
   {cmdPrimary :: CommandName
    -- ^This should not include the prefix (usually a backslash).
   ,cmdAliases :: [CommandName]
    -- ^Ditto for these
   ,cmdDoc :: DocString
   ,cmdArguments :: Arguments
   ,cmdFunction :: StringFunction}

-- |Arguments
data Argument =
  Argument {argumentName :: String
           ,argumentDocumentation :: DocString
           ,argumentDefault :: Maybe String}
  deriving (Eq, Show)

type Arguments = [Argument]
type ArgumentMap = HashMap String String

-- |An environment is a bit more involved
data Environment =
  Environment {envPrimary :: EnvironmentName
              ,envAliases :: [EnvironmentName]
              ,envDoc :: DocString
              ,envArguments :: Arguments
               -- |An environment *must* have some input. I.e. it has to do
               -- something with the stuff between @\begin{environment}@ and
               -- @\end{environment}@.
               --
               -- It can optionally require more arguments, but it must document
               -- them =p.
              ,envFunction :: String -> StringFunction}

-- |Pretty self-explanatory
data Delimiters =
  Delimiters {commandPrefix :: String
             ,lineCommentPrefix :: String
             ,blockCommentPrefix :: String
             ,blockCommentSuffix :: String
             ,bracketStart :: String
             ,bracketEnd :: String
             ,bracketSep :: String}

-- |> Delimiters "\\" "//" "/*" "*/" "{" "}" ","
instance Default Delimiters where
  def = Delimiters "\\" "//" "/*" "*/" "{" "}" ","

-- |The main type for the parser.
data DocumentPart
  = Comment String
  | Ignore String
  | CommandCall CommandName [MKV]
  | EnvironmentCall EnvironmentName String [MKV]
  deriving (Eq, Show)

-- |The type for arguments in function calls. This will later be marshaled into
-- an 'ArgumentMap'.
data MKV
  = Positional String
  | WithKey String String
  deriving (Eq, Show)

-- |A text function
type StringFunction = ArgumentMap -> DocumentM Pandoc

-- Marshal a bunch of 'MKV's into an 'ArgumentMap', using the given 'Arguments'
-- as a reference.
mkArgMap :: [MKV] -> Arguments -> Exceptional ArgumentMap
mkArgMap mkvs args' =
  do (result,(_,remainingArguments)) <-
       runStateT (do texts <- traverse mkHashMapEntry mkvs
                     return (foldMap (uncurry H.singleton) texts))
                 (0,args')
     rest <-
       for remainingArguments
           (\arg ->
              case argumentDefault arg of
                Nothing ->
                  fail (mappend "Unbound argument with no default: " (show arg))
                Just x ->
                  return (H.singleton (argumentName arg)
                                      x))
     return (mappend result (foldMap id rest))
  where mkHashMapEntry
          :: MKV -> StateT (Int,Arguments) Exceptional (String,String)
        mkHashMapEntry =
          \case
            -- If we are given a positional argument, consume the leading
            -- argument in the Arguments, assign the appropriate kv pair,
            -- then continue.
            Positional value ->
              do (i,args'') <- get
                 case headMay args'' of
                   -- If there aren't any arguments left to consume, then send an error message
                   Nothing ->
                     lift (Failure (mconcat ["Too many positional arguments sent to the function! I got at least "
                                            ,show i
                                            ," (counting starts at 0)."]))
                   -- If there is an argument, consume it, assign the appropriate KV pair, and increment the counter
                   Just arg ->
                     do put (i + 1,tail args'')
                        pure (argumentName arg,value)
            -- If there is a key-value pair, assign the key-value pair. If an
            -- argument with that key happens to be listed in the Arguments,
            -- kindly remove it.
            --
            -- The reasons we don't throw errors if the key does not appear in the
            -- list of Arguments:
            --
            --   * This allows the user to override a positional value with a key-value pair
            --   * This allows the user to send arbitrary key-values to the function
            --   * This allows the user to specify an argument out-of-position
            WithKey k v ->
              do (i,args'') <- get
                 case findIndex ((== k) . argumentName) args'' of
                   Nothing -> return ()
                   Just x ->
                     put (i
                         ,mappend (take x args'')
                                  (drop (x + 1) args''))
                 pure (k,v)

-- *** Semantic aliases for 'String'
type DocString = String
type CommandName = String
type EnvironmentName = String
type MarkdownString = String
