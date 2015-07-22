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
-- Module      : Text.Comarkdown.Combinators
-- Description : Fancy combinators for comarkdown
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module Text.Comarkdown.Combinators where

import Text.Comarkdown.Types

import Control.Exceptional
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

-- |This inserts a command into the document state. If such a command already
-- exists, it will return an error message.
-- 
-- Since: 0.1.0.0
newCommand :: (MonadState Document m,ToTextFunction t)
           => CommandName
           -> [CommandName]
           -> DocString
           -> t
           -> m (Exceptional ())
newCommand prim als doc fn =
  do oldState <- get
     let newcmd =
           Command prim
                   (V.fromList als)
                   doc
                   (toTextFunction fn)
         oldcmds = definedCommands oldState
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
newEnvironment :: (MonadState Document m,ToTextFunction t)
               => EnvironmentName
               -> [EnvironmentName]
               -> DocString
               -> (Text -> t)
               -> m (Exceptional ())
newEnvironment prim als doc fn =
  do oldState <- get
     let newenv = Environment prim (V.fromList als) doc (toTextFunction . fn)
         oldenvs = definedEnvironments oldState
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
