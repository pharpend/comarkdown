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
  , module Text.Comarkdown
  ) where

import Control.Exceptional
import Control.Monad.State
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
   {cmdPrimary :: Text
   ,
    -- |Ditto for these
    cmdAliases :: Vector Text
   ,cmdDoc :: DocString
   ,cmdFunction :: TextFunction}

-- |An environment is a bit more involved
data Environment =
  Environment {envPrimary :: Text
              ,envAliases :: Vector Text
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

-- |Semantic alias for 'Text'
type DocString = Text

class ToTextFunction a where
  toTextFunction :: DocString -> a -> TextFunction

instance ToTextFunction Text where
  toTextFunction = Result

instance ToTextFunction t => ToTextFunction (Text -> t) where
  toTextFunction d f = 
    -- Dear HLint, this is much easier to understand. I know that it can be
    -- written pointfree. Congratulations on figuring that out.
    MoreInput d (\x -> toTextFunction d (f x))

-- |This inserts a command into the document state. If such a command already
-- exists, it will return an error message.
-- 
-- If you don't care about collisions, use 'newCommand\''
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

-- |Insert a command into the document state. This does not bother to check if
-- such a command already exists. If you want to avoid collisions, use
-- 'newCommand'
newCommand' :: (MonadState DocumentState m)
            => Command -> m ()
newCommand' newcmd =
  do oldState <- get
     -- I do get the point of lens, now
     put (oldState {definedCommands =
                      (V.cons newcmd (definedCommands oldState))})

-- |This inserts a environment into the document state. If such a environment already
-- exists, it will return an error message.
-- 
-- If you don't care about collisions, use 'newEnvironment\''
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

-- |Insert a environment into the document state. This does not bother to check if
-- such a environment already exists. If you want to avoid collisions, use
-- 'newEnvironment'
newEnvironment' :: (MonadState DocumentState m)
                => Environment -> m ()
newEnvironment' newenv =
  do oldState <- get
     -- I do get the point of lens, now
     put
       (oldState {definedEnvironments =
                    (V.cons newenv (definedEnvironments oldState))})
