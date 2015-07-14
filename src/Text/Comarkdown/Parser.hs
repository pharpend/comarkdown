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

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Vector as V
import Text.Parsec hiding (parse)

-- |Internal type for parsers
type Parser = ParsecT ByteString DocumentState IO

-- |The state of the parser
data DocumentState 
  = -- |Everything appears fine and dandy
    OK
  | -- |We've started consuming input, but were in the middle of parsing an
    -- expression when suddenly eof
    Incomplete IntermediateResult

-- IntermediateResult
data IntermediateResult
  = Parsing

-- |Parse a lazy 'ByteString'. 'SourceName' is an alias for 'String'.
comdParse :: SourceName
          -> ByteString
          -> IO (Either String Document)
comdParse sn bs =
  runParserT comdParser OK sn bs >>=
  \case
    Left x -> return (Left (show x))
    Right x -> return (Right x)

-- |Parse a file. This is equivalent to 'parse' with the 'SourceName' as the
-- 'FilePath'
comdParseFile :: FilePath -> IO (Either String Document)
comdParseFile fp = B.readFile fp >>= comdParse fp

-- |Parse a 'Document'
comdParser :: Parser Document
comdParser =
  fmap (V.fromList . mconcat)
       (many1 (manyTill (altWith "Parsing of macros and such" [])
                        (many1 nonsense <|> eof *> pure [EmptyPart])) <|>
        eof *> pure [[EmptyPart]])

-- |This runs through a list of parsers, and has a message if all of them fail.
altWith :: String -> [Parser x] -> Parser x
altWith s xs = alt xs <?> s

-- |Go through a list of parsers, failing if and only if all of them fail. None
-- of the parsers will consume input if they fail.
alt :: [Parser x] -> Parser x
alt zs =
  case zs of
    [] -> parserZero
    x:xs -> try x <|> alt xs

-- |Whitespace, comments, or an ignore block
nonsense :: Parser ComdPart
nonsense =
  altWith "Whitespace and such"
          [whiteSpace,lineComment,blockComment,ignoreBlock]

-- |Whitespace
whiteSpace :: Parser ComdPart
whiteSpace = many1 space *> pure EmptyPart <?> "Whitespace"

-- |The string "//", followed by anything until a newline or EOF.
lineComment :: Parser ComdPart
lineComment = 
  flabel "Line comment" $
  do skipMany hspace
     string "//"
     commentText <- manyTill anyChar (try eol)
     return (Comment (T.pack commentText))

-- |A C-style block comment. The string "/*" followed by anything until "*/"
blockComment :: Parser ComdPart
blockComment =
  flabel "Block comment" $
  do skipMany hspace
     string "/*"
     commentText <- manyTill anyChar 
                             (try (string "*/"))
     return (Comment (T.pack commentText))
     
-- |Any one of the following syntaxes:
-- 
-- > \ignore
-- > ...
-- > \unignore
-- 
-- > \ignoreTill{foo}
-- > ...
-- > foo
-- 
-- Note that this does not cover this syntax, because it is a special
-- environment.
-- 
-- > \begin{ignore}
-- > ...
-- > \end{ignore}
ignoreBlock :: Parser ComdPart
ignoreBlock = altWith "Ignore block"
                      [normalIgnoreSyntax <?> "\\ignore syntax"
                      ,ignoreTillSyntax <?> "\\ignoreTill syntax"]
  where normalIgnoreSyntax = 
          do string "\\ignore"
             ignoreThis <- manyTill anyChar (try (string "\\unignore"))
             return (Ignore (T.pack ignoreThis))
        ignoreTillSyntax =
          do string "\\ignoreTill"
             skipMany space
             char '{'
             keyword <- manyTill anyChar (char '}')
             ignoreThis <- manyTill anyChar (try (string keyword))
             return (Ignore (T.pack ignoreThis))

-- |Alias for @flip label@
flabel :: String -> Parser x -> Parser x
flabel = flip label

-- |Horizontal space. This interprets spaces and tabulators as horizontal space.
hspace :: Parser Char
hspace = altWith "Horizontal space" 
                 [char ' ' <?> "Space character"
                 ,char '\t' <?> "Tab character"]

-- |Line break. Note that this also interprets end-of-file as a line break
eol :: Parser ()
eol = altWith "Line break or end of file"
              [pure () <* string "\r\n" <?> "Carriage return plus newline"
              ,pure () <* string "\r" <?> "Bare carriage return"
              ,pure () <* string "\n" <?> "Bare newline"
              ,eof <?> "End of input"]
