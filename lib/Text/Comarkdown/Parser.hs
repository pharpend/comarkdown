{-# LANGUAGE Safe #-}

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
-- Description : Parsers for Comarkdown
-- Copyright   : Copyright 2015 Peter Harpending.
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : portable
-- 
-- This module contains <https://hackage.haskell.org/package/parsec Parsec>
-- parsers for Comarkdown.
-- 
-- In most cases, you do not need to import this module, as everything herein is
-- exported by "Text.Comarkdown".

module Text.Comarkdown.Parser where

import Data.ByteString (ByteString)
import Text.Comarkdown.Types
import Text.Parsec hiding (parse)
import qualified Text.Parsec as P

-- |A shortcut type for parsers
type Parser = Parsec ByteString DocumentState

-- |In the future, we'll have a richer state, but for now...
type DocumentState = ()

-- |Parse a comarkdown document
parse :: ByteString -> Maybe Document
parse = undefined

-- |Parse a comarkdown document, returning an error message on a parse failure.
parseEither :: ByteString -> Either String Document
parseEither = undefined

-- |Parse a comarkdown document, returning a 'ParseError' on a parse failure.
parseEither' :: ByteString -> Either ParseError Document
parseEither' = undefined

-- |Parse a comarkdown document from a file
parseFile :: FilePath -> Maybe Document
parseFile = undefined

-- |Parse a comarkdown document from a file, returning an error message on a
-- parse failure.
parseFileEither :: FilePath -> Either String Document
parseFileEither = undefined

-- |Parse a comarkdown document from a file, returning a 'ParseError' on a parse
-- failure.
parseFileEither' :: FilePath -> Either ParseError Document
parseFileEither' = undefined

-- |The Parsec 'Parser' for Comarkdown 'Document's
comarkdownParser :: Parser Document
comarkdownParser = undefined
