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

import Control.Monad.Extra ((>>$))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Text.Comarkdown.Types
import Text.Parsec hiding (parse)

-- |A shortcut type for parsers
type Parser = ParsecT Text DocumentState IO

-- |In the future, we'll have a richer state, but for now...
data DocumentState =
  DocumentState {indentLevel :: Int}
  
-- |The default document state
nullDocumentState :: DocumentState
nullDocumentState = DocumentState {indentLevel = 0}

-- |Parse a comarkdown document, returning an error message on a parse failure.
-- 
-- I'll save you a click: 'SourceName' is a semantic alias for 'String'
parse :: SourceName -> Text -> IO (Either String Document)
parse sn =
  parse' sn >>$
  \case
    Left x -> Left (show x)
    Right x -> Right x

-- |Parse a comarkdown document, returning a 'ParseError' on a parse failure.
parse' :: SourceName -> Text -> IO (Either ParseError Document)
parse' sn bs = runParserT comarkdownParser nullDocumentState sn bs

-- |Parse a comarkdown document from a file, returning an error message on a
-- parse failure.
parseFile :: FilePath -> IO (Either String Document)
parseFile =
  parseFile' >>$
  \case
    Left x -> Left (show x)
    Right x -> Right x

-- |Parse a comarkdown document from a file, returning a 'ParseError' on a parse
-- failure.
parseFile' :: FilePath -> IO (Either ParseError Document)
parseFile' fp = T.readFile fp >>= parse' fp

-- |The Parsec 'Parser' for Comarkdown 'Document's
comarkdownParser :: Parser Document
comarkdownParser = fail "not implemented"

-- -- |Parser for command definitions
-- defCommandParser :: Parser DefMacro
-- defCommandParser = do char '\\'
--                       cmdName <- variableName
--                       skipMany space
--                       char '{'
--                       skipMany space
--                       vnd <- varNamesDefaults
--                       skipMany space
--                       char '}'
--                       skipMany space
--                       body <- between (char '{') (char '}')
--                       return (DefCommand cmdName vnd body)
--    varNamesDefaults :: Parser (Map )

-- |Parser for variable names
-- 
-- Anything is a valid variable name as long as it's not a space.
variableName :: Parser Text
variableName = fmap T.pack $
               do skipMany space
                  initChar <- anyChar
                  rest <- manyTill anyChar (try space)
                  return (initChar:rest)
