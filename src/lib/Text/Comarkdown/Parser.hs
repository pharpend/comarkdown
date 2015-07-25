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
type DocParseM x = ParsecT ByteString Document IO x

-- |Try to parse a bunch of document parts from a ByteString
documentParser :: DocParseM (Vector DocumentPart)
documentParser =
  label' "document" $
  do maybeFirst <- optionMaybe interestingPart
     rest <-
       sepEndBy (fmap (Ignore . T.pack)
                      (many1 anyChar))
                interestingPart
     return (V.fromList
               (case maybeFirst of
                  Nothing -> rest
                  Just fst' -> fst' : rest))
  where interestingPart :: DocParseM DocumentPart
        interestingPart =
          try explicitIgnore <|> try lineComment <|> try blockComment <|>
          try environmentCall <|>
          try commandCall 

-- |Parse a line comment.
lineComment :: DocParseM DocumentPart
lineComment =
  label' "line comment" $
  do dels <- fmap delimiters getState
     text (lineCommentPrefix dels)
     commentStr <- manyTill anyChar eol
     return (Comment (T.pack commentStr))

-- |Parse a block comment
blockComment :: DocParseM DocumentPart
blockComment =
  label' "block comment" $
  do delims <- fmap delimiters getState
     text (blockCommentPrefix delims)
     commentStr <- manyTill anyChar (try (text (blockCommentSuffix delims)))
     return (Comment (T.pack commentStr))

-- |Parse an environment call
environmentCall :: DocParseM DocumentPart
environmentCall =
  label' "environment call" $
  do (envName,envArgs) <- beginEnv <?> "environment name and extra arguments"
     envBody' <- envBody envName
     -- Note that, for now, envArgs will always be empty
     return (EnvironmentCall envName envBody' envArgs)
  where beginEnv =
          do delims <- fmap delimiters getState
             text (commandPrefix delims)
             many space
             text "begin"
             many space
             bracketStart'
             envName <-
               label' "environment name" (manyTill anyChar (try bracketEnd'))
             return (T.pack envName,mempty)
        envBody :: Text -> DocParseM Text
        envBody nom =
          T.pack <$>
          manyTill anyChar
                   (try (do delims <- fmap delimiters getState
                            text (commandPrefix delims)
                            many space
                            text "end"
                            many space
                            bracketStart'
                            text nom
                            bracketEnd'))

-- |Parse a command call
commandCall :: DocParseM DocumentPart
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
     return (CommandCall cmdName args')

-- Parse arguments within the delimiters. This also parses the open and close
-- braces
args :: DocParseM (Vector MKV)
args =
  label' "arguments in brackets" $
  do bracketStart'
     args'
  where val :: DocParseM Text
        val =
          do firstChar <- anyChar
             restOfChars <-
               manyTill anyChar (try bracketSep' <|> try bracketEnd')
             return T.pack (firstChar : restOfChars)
        args' :: DocParseM Vector MKV
        args' =
          do key <-
               optionMaybe
                 (do firstChar <- anyChar
                     restOfChars <- manyTill anyChar (space <|> char '=')
                     char '='
                     return (T.pack (firstChar : restOfChars)))
             val' <- val
             -- If there's a comma, parse more stuff
             rest <-
               optionMaybe
                 (do bracketSep'
                     args')
             -- Otherwise, we're done
             bracketEnd'
             return (V.cons (case key of
                               Nothing -> Positional val'
                               Just x -> WithKey x val')
                            (case rest of
                               Nothing -> mempty
                               Just x -> x))

data MKV
  = Positional Text
  | WithKey Text
            Text

-- |Parse an explicit ignore block
explicitIgnore :: DocParseM DocumentPart
explicitIgnore =
  label' "explicit ignore" $
  do delims <- fmap delimiters getState
     text (commandPrefix delims)
     many space
     text "ignore"
     ignoreThis <-
       manyTill anyChar
                (try (do text (commandPrefix delims)
                         many space
                         text "unignore"))
     return (Ignore (T.pack ignoreThis))

-- * Helper functions

bracketStart' :: DocParseM ()
bracketStart' =
  label' "start bracket" $
  do brs <- fmap (bracketStart . delimiters) getState
     text brs
     many space
     return ()

bracketEnd' :: DocParseM ()
bracketEnd' =
  label' "end bracket" $
  do many space
     brs <- fmap (bracketEnd . delimiters) getState
     text brs
     return ()

bracketSep' :: DocParseM ()
bracketSep' =
  label' "argument separator" $
  do many space
     brs <- fmap (bracketSep . delimiters) getState
     text brs
     many space
     return ()

-- |Parse an end-of-line or end-of-file
eol :: DocParseM ()
eol =
  label' "end of line" $
  try_ (text "\r\n") <|> try_ (text "\r") <|> try_ (text "\n") <|> eof
  where try_ x =
          do try x
             return ()

-- |Same as 'label' with the arguments flipped
label' :: String -> DocParseM a -> DocParseM a
label' = flip label

-- |Wrapper around 'string'
text :: Text -> DocParseM Text
text = fmap T.pack . string . T.unpack
