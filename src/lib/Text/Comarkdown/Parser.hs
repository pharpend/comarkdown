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
-- Description : The Comarkdown parser
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module Text.Comarkdown.Parser where

import Text.Comarkdown.Types

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Parsec

-- |Convenient alias for the particular parser monad
type DocumentM x = ParsecT ByteString Document IO x

-- |Try to parse a bunch of document parts from a ByteString
documentParser :: DocumentM (Vector DocumentPart)
documentParser = label' "document" $ fmap V.fromList (many part)
  where part =
          do maybeFirst <- optionMaybe interestingPart
             rest <- sepEndBy (many1 anyChar) interestingPart
             return (V.fromList (case maybeFirst of 
                                   Nothing -> rest
                                   Just fst -> fst : rest))
        interestingPart =
          try lineComment <|> try blockComment <|> try environmentCall <|>
          try commandCall

-- |Parse a line comment.
lineComment :: DocumentM DocumentPart
lineComment =
  label' "line comment" $
  do dels <- fmap delimiters getState
     text (lineCommentPrefix dels)
     commentStr <- manyTill anyChar eol
     return (Comment (T.pack commentStr))

-- |Parse a block comment
blockComment :: DocumentM DocumentPart
blockComment =
  label' "block comment" $
  do delims <- fmap delimiters getState
     text (blockCommentPrefix delims)
     commentStr <- manyTill anyChar (try (text (blockCommentSuffix delims)))
     return (Comment (T.pack commentStr))

-- |Parse an environment call
environmentCall :: DocumentM DocumentPart
environmentCall =
  label' "environment call" $
  do delims <- fmap delimiters getState
     -- Note that, for now ,envArgs will always be empty
     (envName,envArgs) <- beginEnv <?> "environment name and extra arguments"
     envBody' <- envBody envName
     return (EnvironmentCall envName envBody' envArgs)
  where beginEnv =
          do delims <- fmap delimiters getState
             text (commandPrefix delims)
             text "begin"
             many space
             bracketStart'
             envName <-
               label' "environment name" (manyTill anyChar (try bracketEnd'))
             return (T.pack envName,mempty)
        envBody nom =
          manyTill anyChar
                   (try (do delims <- fmap delimiters getState
                            text (commandPrefix delims)
                            text "end"
                            bracketStart'
                            text nom
                            bracketEnd'))

-- |Parse a command call
commandCall :: DocumentM DocumentPart
commandCall =
  label' "command call" $
  do delims <- fmap delimiters getState
     text (commandPrefix delims)
     many space
     -- Anything until a space
     fst' <- anyChar
     rest <- manyTill anyChar (try space)
     let cmdName = T.pack (fst' : rest)
     args' <- args
     return (Command cmdName args')

-- Parse arguments within the delimiters. This also parses the open and close
-- braces
args :: DocumentM (Vector Text)
args =
  label' "arguments in brackets" $
  do bracketStart'
     args'
  where args' =
          do firstChar <- anyChar
             restOfChars <-
               manyTill anyChar (try bracketSep' <|> try bracketEnd')
             let thisArg = T.pack (firstChar : restOfChars)
             -- If there's a comma, parse more stuff
             rest <-
               try (do bracketSep'
                       args')
             -- Otherwise, we're done
             bracketEnd'
             return (V.fromList (thisArg : rest))

-- * Helper functions

bracketStart' :: DocumentM ()
bracketStart' =
  label' "start bracket" $
  do brs <- fmap (bracketStart . delimiters) getState
     text brs
     many space
     return ()

bracketEnd' :: DocumentM ()
bracketEnd' =
  label' "end bracket" $
  do many space
     brs <- fmap (bracketEnd . delimiters) getState
     text brs
     return ()

bracketSep' :: DocumentM ()
bracketSep' =
  label' "argument separator" $
  do many space
     brs <- fmap (bracketSep . delimiters) getState
     text brs
     many space
     return ()

-- |Parse an end-of-line or end-of-file
eol :: DocumentM Text
eol =
  label' "end of line" $
  try (text "\r\n") <|> try (text "\r") <|> try (text "\n") <|> eof

-- |Same as 'label' with the arguments flipped
label' :: String -> DocumentM a -> DocumentM a
label' = flip label

-- |Wrapper around 'string'
text :: Text -> DocumentM Text
text = fmap T.pack . string . T.unpack
