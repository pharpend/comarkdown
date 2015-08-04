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

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Foldable
import qualified Data.HashMap.Lazy as H
import Text.Comarkdown
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  context
    "Text.Comarkdown.Types" $ do spec_toCf

spec_toCf :: Spec
spec_toCf =
  context "toCf function" $ do
    context "given list of commands, produce CF where cmds <- cfCommands" $ do
      specify "with stdlib" $ fmap fst $ flip runStateT nullDocument $ withStdlib $ do
        st <- get
        forM_ (foldMap (\s -> cmdPrimary s : cmdAliases s) (definedCommands st)) $ \c ->
          liftIO $ shouldSatisfy c $ flip elem (H.keys (cfCommands (toCf st)))
      specify "with a randomly generated list of commands" $ property $ do
        length' <- generate (suchThat arbitrary (> 0)) :: IO Int
        foo <- foldlM
                 (\accum _ -> do
                    baz <- generate (suchThat arbitrary (\x -> (notElem x accum) && (length x > 0)))
                    pure (baz : accum))
                 mempty
                 [1 .. length']
        bar <- foldlM
                 (\accum _ -> do
                    baz <- generate (suchThat arbitrary (all (\x -> (all (notElem x) accum)
                                                                    && (length x > 0))))
                    pure (baz : accum))
                 mempty
                 [1 .. length']
        let cmds = do
              (n, ns) <- zip foo bar
              return (Command n ns mempty mempty (\_ -> pure mempty))
        _ <- runStateT
               ((do
                   st <- get
                   commands <>= cmds
                   let cmdKeys =
                         H.keys (cfCommands (toCf st))
                   liftIO $ do
                     shouldSatisfy foo (all (`elem` cmdKeys))
                     forM_ bar $ \x -> shouldSatisfy x (all (`elem` cmdKeys))) :: DocumentM ())
               nullDocument
        return ()
