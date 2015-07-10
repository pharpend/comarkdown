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
-- Module      : Text.Comarkdown.Parser
-- Description : The parser for Comarkdown
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module Text.Comarkdown.Parser where

import Text.Comarkdown.Types

import Data.ByteString.Lazy (ByteString)
import Text.Parsec

parse :: SourceName
      -> ByteString
      -> IO (Either String Document)
parse sn bs =
  runParserT comdParser () sn bs >>=
  \case
    Left x -> return (Left (show x))
    Right x -> return (Right x)

runDocument :: Document -> ByteString
runDocument _ = mempty

comdParser :: Parser Document
comdParser = return mempty

type Parser = ParsecT ByteString () IO
