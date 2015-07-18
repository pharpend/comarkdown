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
import Control.Monad
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

-- |A command has a list of keywords, along with documentation.
data Command =
  Command {cmdPrimary :: Text
          ,cmdAliases ::  Vector Text
          ,cmdExpandTo :: TextFunction
          ,cmdSynopsis :: Text
          ,cmdDescription :: Text}
  deriving (Show,Eq)

-- |A function which either produces a result or demands more input
data TextFunction = Result Text
                  | MoreInput Text TextFunction
  deriving (Eq, Show)

-- |The document state has a list of definitions
data DocumentState =
  DocumentState {definedCommands :: Vector Command}
  deriving (Show,Eq)

-- |This inserts a command into the document state. If such a command already
-- exists, it will return an error message.
-- 
-- If you don't care about collisions, use 'newCommand\''
newCommand :: (MonadState DocumentState m)
           => Command -> m (Exceptional ())
newCommand newcmd =
  do oldcmds <- fmap definedCommands get
     -- Test to see if any of cmd's tokens are a token of another command
     let proveThereAreNoCollisions =
           forM_ oldcmds $
           \oldcmd ->
             let newprim = cmdPrimary newcmd
                 newals = cmdAliases newcmd
                 oldprim = cmdPrimary oldcmd
                 oldals = cmdAliases oldcmd
             in if |  newprim == oldprim ->
                     fail (mconcat ["Primary name "
                                   ,T.unpack newprim
                                   ," is already in use."])
                   |  newprim `elem` oldals ->
                     fail (mconcat [T.unpack newprim
                                   ," is already an alias of "
                                   ,T.unpack oldprim
                                   ,"."])
                   |  otherwise ->
                     forM_ newals $
                     \newal ->
                       if |  newal == oldprim ->
                            fail (mconcat ["Alias "
                                          ,T.unpack newal
                                          ," is the name of an existing command."])
                          |  newal `elem` oldals ->
                            fail (mconcat ["Alias "
                                          ,T.unpack newal
                                          ," is already an alias of "
                                          ,T.unpack oldprim])
                          |  otherwise -> pure ()
     -- After all that is done...
     case proveThereAreNoCollisions of
       -- If there was a collision, return an error message
       Failure s -> return $ Failure s
       -- If there weren't any collisions, just modify the state
       Success _ -> fmap Success (put (DocumentState (V.cons newcmd oldcmds)))

-- |Insert a command into the document state. This does not bother to check if
-- such a command already exists. If you want to avoid collisions, use 'newCommand'
newCommand' :: (MonadState DocumentState m)
            => Command -> m ()
newCommand' newcmd =
  do oldcmds <- fmap definedCommands get
     put (DocumentState (V.cons newcmd oldcmds))
