{-# LANGUAGE LambdaCase #-}

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

import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Text.Comarkdown.Types
import Text.Parsec hiding (parse)

-- |A shortcut type for parsers
-- 
-- Since: 0.1.0.0
type Parser = ParsecT ByteString DocumentState IO

-- |In the future, we'll have a richer state, but for now...
-- 
-- Since: 0.1.0.0
type DocumentState = ()

-- |Parse a comarkdown document, returning an error message on a parse failure.
-- 
-- I'll save you a click: 'SourceName' is a semantic alias for 'String'
-- 
-- Since: 0.1.0.0
parse :: SourceName -> ByteString -> IO (Either String Document)
parse sn =
  parse' sn >>$
  \case
    Left x -> Left (show x)
    Right x -> Right x

-- |Parse a comarkdown document, returning a 'ParseError' on a parse failure.
-- 
-- Since: 0.1.0.0
parse' :: SourceName -> ByteString -> IO (Either ParseError Document)
parse' sn bs = runParserT comarkdownParser () sn bs

-- |Parse a comarkdown document from a file, returning an error message on a
-- parse failure.
-- 
-- Since: 0.1.0.0
parseFile :: FilePath -> IO (Either String Document)
parseFile =
  parseFile' >>$
  \case
    Left x -> Left (show x)
    Right x -> Right x

-- |Parse a comarkdown document from a file, returning a 'ParseError' on a parse
-- failure.
-- 
-- Since: 0.1.0.0
parseFile' :: FilePath -> IO (Either ParseError Document)
parseFile' fp =
  do fileBytes <- BS.readFile fp
     let lazyFileBytes = BL.fromStrict fileBytes
     parse' fp lazyFileBytes

-- |The Parsec 'Parser' for Comarkdown 'Document's
-- 
-- Since: 0.1.0.0
comarkdownParser :: Parser Document
comarkdownParser = undefined

-- |Convenience function. It's essentially like '(>=>)', but with a pure
-- function as the second argument
-- 
-- Since: 0.1.0.0
(>>$) :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
(>>$) monadAction mapper monadVal =
  fmap mapper (monadAction monadVal)
