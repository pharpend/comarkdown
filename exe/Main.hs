{-# LANGUAGE CPP #-}

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
-- Module      : Main
-- Description : The @comd@ executable
-- Copyright   : Copyright 2015 Peter Harpending.
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module Main where

#if __GLASGOW_HASKELL < 710
import Control.Applicative
import Data.Monoid
#endif
import Data.Version (showVersion)
import Paths_comarkdown (version)
import Options.Applicative

main :: IO ()
main =
  customExecParser parserPrefs parserInfo >>= runArgs
  where parserPrefs =
          ParserPrefs "" True True True 80
        parserInfo =
          info (helper <*> parser)
               (mconcat [fullDesc,progDesc "Compiler for Comarkdown"])
        parser =
          flag' Version (mconcat [long "version",help "Show the version"])

data Args = Version

runArgs :: Args -> IO ()
runArgs Version = putStrLn $ showVersion version
