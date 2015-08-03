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
-- Module      : TypesSpec
-- Description : Tests for Text.Comarkdown.Types
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module TypesSpec where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.HashMap.Lazy as H
import qualified Data.Vector as V
import Text.Comarkdown
import Test.Hspec
-- import Test.QuickCheck

spec :: Spec
spec = 
  context "Text.Comarkdown.Types" $
  do spec_toCf

spec_toCf :: Spec
spec_toCf =
  context "toCf function" $
  context "given list of commands, produce CF where cmds <- cfCommands" $
  do specify "with stdlib" $
       fmap fst $
       flip runStateT nullDocument $
       withStdlib $
       do st <- get
          forM_ (foldMap (\s ->
                            V.cons (cmdPrimary s)
                                   (cmdAliases s))
                         (definedCommands st)) $
            \c ->
              liftIO $
              shouldSatisfy c $ flip elem (H.keys (cfCommands (toCf st)))
     specify "with a randomly generated list of commands" $
       pendingWith "Test is a pain in the ass to write."
       -- property $
       -- \(foo :: [String],bar :: [[String]]) ->
       --   (do let cmds = do (n,ns) <- zip foo bar
       --                     return (Command n 
       --                                     (V.fromList ns) 
       --                                     mempty 
       --                                     mempty 
       --                                     (\_ -> fromPandoc' (readMarkdown def "Yay")))
       --       _ <- runStateT ((do st <- get
       --                           put (st { definedCommands = mappend (definedCommands st) 
       --                                                               (V.fromList cmds)})
       --                           let cmdKeys = H.keys (cfCommands (toCf st))
       --                           liftIO $
       --                             do shouldSatisfy foo (all (`elem` cmdKeys))
       --                                forM_ bar $
       --                                  \x -> shouldSatisfy x (all (`elem` cmdKeys))) 
       --                       :: DocumentM ())
       --                      nullDocument
       --       return ()) :: IO ()


