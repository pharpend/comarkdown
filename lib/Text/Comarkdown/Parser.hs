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

import Control.Monad.IO.Class
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Parsec

-- |Convenient alias for the particular parser monad
type DocParseM x = ParsecT String Document IO x

-- |Try to parse a bunch of document parts from a ByteString
documentParser :: DocParseM (Vector DocumentPart)
documentParser =
  label' "document" $
  do maybeFirst <- optionMaybe interestingPart
     rest <-
       sepEndBy (fmap Ignore
                      (many1 anyChar))
                interestingPart
     return (V.fromList
               (case maybeFirst of
                  Nothing -> rest
                  Just fst' -> fst' : rest))

-- |Everything but implicit Ignore blocks
interestingPart :: DocParseM DocumentPart
interestingPart =
  try explicitIgnore <|> try lineComment <|> try blockComment <|>
  try environmentCall <|>
  try commandCall 

-- |Parse a line comment.
lineComment :: DocParseM DocumentPart
lineComment =
  label' "line comment" $
  do dels <- fmap delimiters getState
     string (lineCommentPrefix dels)
     commentStr <- manyTill anyChar eol
     return (Comment commentStr)

-- |Parse a block comment
blockComment :: DocParseM DocumentPart
blockComment =
  label' "block comment" $
  do delims <- fmap delimiters getState
     string (blockCommentPrefix delims)
     commentStr <- manyTill anyChar (try (string (blockCommentSuffix delims)))
     return (Comment commentStr)

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
             string (commandPrefix delims)
             many space
             string "begin"
             many space
             bracketStart'
             envName <-
               label' "environment name" (manyTill anyChar (try bracketEnd'))
             return (envName,mempty)
        envBody :: String -> DocParseM String
        envBody nom =
          manyTill anyChar
                   (try (do delims <- fmap delimiters getState
                            string (commandPrefix delims)
                            many space
                            string "end"
                            many space
                            bracketStart'
                            string nom
                            bracketEnd'))

-- |Parse a command call
commandCall :: DocParseM DocumentPart
commandCall =
  label' "command call" $
  do delims <- fmap delimiters getState
     string (commandPrefix delims)
     many space
     -- Anything until a space or bracket start
     fst' <- anyChar
     rest <- manyTill anyChar ((space >> pure ()) <|> lookAhead bracketStart')
     let cmdName = fst' : rest
     args' <- many arg
     return (CommandCall cmdName (V.fromList args'))
  where arg = do many space
                 bracketStart'
                 foo <- manyTill anyChar bracketEnd'
                 many space
                 return (Positional foo)

-- |Parse an explicit ignore block
explicitIgnore :: DocParseM DocumentPart
explicitIgnore =
  label' "explicit ignore" $
  do delims <- fmap delimiters getState
     string (commandPrefix delims)
     many space
     string "ignore"
     ignoreThis <-
       manyTill anyChar
                (try (do string (commandPrefix delims)
                         many space
                         string "unignore"))
     return (Ignore ignoreThis)

-- * Helper functions

bracketStart' :: DocParseM ()
bracketStart' =
  label' "start bracket" $
  do brs <- fmap (bracketStart . delimiters) getState
     string brs
     many space
     return ()

bracketEnd' :: DocParseM ()
bracketEnd' =
  label' "end bracket" $
  do many space
     brs <- fmap (bracketEnd . delimiters) getState
     string brs
     return ()

bracketSep' :: DocParseM ()
bracketSep' =
  label' "argument separator" $
  do many space
     brs <- fmap (bracketSep . delimiters) getState
     string brs
     many space
     return ()

-- |Parse an end-of-line or end-of-file
eol :: DocParseM ()
eol =
  label' "end of line" $
  try_ (string "\r\n") <|> try_ (string "\r") <|> try_ (string "\n") <|> eof
  where try_ x =
          do try x
             return ()

-- |Same as 'label' with the arguments flipped
label' :: String -> DocParseM a -> DocParseM a
label' = flip label
