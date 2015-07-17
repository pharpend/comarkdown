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
-- Module      : Text.Comd
-- Description : The library module for comarkdown
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module Text.Comd where

import Control.Monad.Trans.Class

-- * Fundamentals
-- 
-- ** The preprocessor monad
-- 
-- Comarkdown can be thought of as a preprocessor. It reads a file, runs
-- something to it, sends back some output.
-- 
-- So, we can define a type for preprocessors
-- 
-- > type Preprocessor = ByteString -> ByteString
-- 
-- To be safe, we'll actually make it a monad
-- 
-- |Remember, in order for this library to be legal Haskell, there must be
-- unnecessary abstraction.
newtype Monad m => PreprocessorT x m y =
  PreprocessorT {runPreprocessorT :: x -> m y}

instance Monad m => Functor (PreprocessorT x m) where
  fmap f (PreprocessorT foo) = PreprocessorT (fmap f . foo)

instance Monad m => Applicative (PreprocessorT x m) where
  pure x = PreprocessorT (\_ -> pure x)
  -- Let's work this out
  -- 
  -- f : a -> m (b -> c)
  -- x : a -> m b
  -- \z -> f z <*> x z : a -> m c
  (PreprocessorT f) <*> (PreprocessorT x) = PreprocessorT (\a -> f a <*> x a)

instance Monad m => Monad (PreprocessorT x m) where
  -- This one's easy
  return = pure
  -- This one... well, let's try to work it out.
  --
  -- x : p (a -> m b)
  -- f : b -> p (a -> m c)
  -- Trying to get : p (a -> m c)
  --
  -- z : a
  -- x z : m b
  -- fmap f (x z) : m (p (a -> m c))
  -- unPP it : m (a -> m c)
  -- it z : m (m c)
  -- undoIt : m c
  --
  -- Well, I think that's what I did here
  (PreprocessorT x) >>= f = 
    PreprocessorT (\z -> do foo <- fmap f (x z)
                            let bar = runPreprocessorT foo
                            bar z)

instance MonadTrans (PreprocessorT x) where
  -- Yay more type tetris
  -- 
  -- x : m b
  -- trying to get: p (a -> m b)
  lift x = PreprocessorT (\_ -> x)
