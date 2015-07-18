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
-- Module      : Text.Comarkdown
-- Description : The entire comarkdown library in one module
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module Text.Comarkdown 
  ( module Control.Exceptional
  , def
  , module Text.Comarkdown
  ) where

import Control.Exceptional
import Control.Monad.State
import Data.Default (def)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

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

-- |Construct a HashMap for efficient lookups of command names.
commandsMap :: DocumentState -> HashMap CommandName TextFunction
commandsMap ds =
  foldl (\accum cmd ->
           let cmdf = cmdFunction cmd
           in mappend (H.insert (cmdPrimary cmd) cmdf accum)
                      (H.fromList
                         [(alias,cmdf) | alias <- V.toList (cmdAliases cmd)]))
        mempty
        (definedCommands ds)

-- |Construct a HashMap for efficient lookups of environment names.
environmentsMap :: DocumentState -> HashMap EnvironmentName (Text -> TextFunction)
environmentsMap ds =
  foldl (\accum env ->
           let envf = envFunction env
           in mappend (H.insert (envPrimary env) envf accum)
                      (H.fromList
                         [(alias,envf) | alias <- V.toList (envAliases env)]))
        mempty
        (definedEnvironments ds)

-- |This inserts a command into the document state. If such a command already
-- exists, it will return an error message.
-- 
-- Since: 0.1.0.0
newCommand :: (MonadState DocumentState m)
           => Command -> m (Exceptional ())
newCommand newcmd =
  do oldState <- get
     let oldcmds = definedCommands oldState
         -- Test to see if any of cmd's tokens are a token of another command
         oldTokens =
           foldl (\stuff cmd ->
                    mappend stuff
                            (V.cons (cmdPrimary cmd)
                                    (cmdAliases cmd)))
                 mempty
                 oldcmds
         errorMessages =
           foldl (\accum token ->
                    if token `elem` oldTokens
                       then V.snoc accum
                                   (mappend (T.unpack token)
                                            " is already in use by another command.")
                       else accum)
                 mempty
                 (V.cons (cmdPrimary newcmd)
                         (cmdAliases newcmd))
     if V.null errorMessages
        then Success <$>
             put (oldState {definedCommands = V.cons newcmd oldcmds})
        else return (Failure (mconcat ["There were errors while trying to make the command "
                                      ,T.unpack (cmdPrimary newcmd)
                                      ,". They are all listed here:"
                                      ,mconcat (V.toList (fmap (mappend "\n    ") errorMessages))]))

-- |This inserts a environment into the document state. If such a environment already
-- exists, it will return an error message.
-- 
-- Since: 0.1.0.0
newEnvironment
  :: (MonadState DocumentState m)
  => Environment -> m (Exceptional ())
newEnvironment newenv =
  do oldState <- get
     let oldenvs = definedEnvironments oldState
         -- Test to see if any of env's tokens are a token of another environment
         oldTokens =
           foldl (\stuff env ->
                    mappend stuff
                            (V.cons (envPrimary env)
                                    (envAliases env)))
                 mempty
                 oldenvs
         errorMessages =
           foldl (\accum token ->
                    if token `elem` oldTokens
                       then V.snoc accum
                                   (mappend (T.unpack token)
                                            " is already in use by another environment.")
                       else accum)
                 mempty
                 (V.cons (envPrimary newenv)
                         (envAliases newenv))
     if V.null errorMessages
        then Success <$>
             put (oldState {definedEnvironments = V.cons newenv oldenvs})
        else return (Failure (mconcat ["There were errors while trying to make the environment "
                                      ,T.unpack (envPrimary newenv)
                                      ,". They are all listed here:"
                                      ,mconcat (V.toList (fmap (mappend "\n    ") errorMessages))]))
