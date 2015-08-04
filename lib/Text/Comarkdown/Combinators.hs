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
-- Module      : Text.Comarkdown.Combinators
-- Description : Fancy combinators for comarkdown
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module Text.Comarkdown.Combinators
  ( (!)
  , module Text.Comarkdown.Combinators
  , module Text.Comarkdown.Combinators.Primitives
  ) where

import Text.Comarkdown.Combinators.Primitives
import qualified Text.Comarkdown.Stdlib as L
import Text.Comarkdown.Parser
import Text.Comarkdown.Types

import Control.Exceptional
import Control.Lens hiding (parts)
import Control.Monad.State
import Data.HashMap.Lazy ((!))
import Data.Monoid ((<>))
import Text.Parsec
import Text.Pandoc

-- ** Missing operators from other modules

-- |Alias for 'mappend'
infixl 5 <+>
(<+>) :: Monoid m => m -> m -> m
(<+>) = mappend

-- * Comarkdown combinators!

-- |Parse a String into the current document.
-- 
-- The source name is required for error messages
parse :: SourceName -> String -> DocumentM ()
parse sn bs =
  do doc <- get
     exceptionalDocument <- liftIO $ parse' doc sn bs
     mDocument <- runExceptional exceptionalDocument
     put mDocument

-- |Parse a String, given an existing document (with definitions and stuff),
-- the name of the source, and a Bytestring to parse.
parse' :: Document -> SourceName -> String -> IO (Exceptional Document)
parse' doc sn bs =
  runParserT documentParser doc sn bs >>=
  return .
  \case
    Left parseError -> fail (show parseError)
    Right parts' -> return (over parts (<> parts') doc)

-- |Parse a file into the current document
parseFile :: (MonadState Document m, MonadIO m) => FilePath -> m ()
parseFile fp =
  do doc <- get
     excNewDoc <- liftIO (parseFile' doc fp)
     mNewDoc <- runExceptional excNewDoc
     put mNewDoc

-- |Runs 'parse\'' on the contents of a file, using the 'FilePath' as the
-- 'SourceName'
parseFile' :: Document -> FilePath -> IO (Exceptional Document)
parseFile' doc fp =
  do contents <- readFile fp
     parse' doc fp contents

-- |Run the document including the 'stdlib'
withStdlib :: DocumentM x -> DocumentM x
withStdlib x = L.stdlib >> x

-- |Wrapper around 'runDocument' and 'stdlib'
runWithStdlib :: DocumentM x -> IO Pandoc
runWithStdlib = runDocument . withStdlib

-- |Parse a comarkdown file, then send the resulting file to Pandoc, write all
-- of this as Markdown
comdToMd :: FilePath -> IO String         
comdToMd fp =                             
  do pandoc <- runWithStdlib $ parseFile fp
     return (writeMarkdown def pandoc)       

-- |Parse a comarkdown file, then send the resulting file to Pandoc, write all
-- of this as plain-text
comdToPlain :: FilePath -> IO String         
comdToPlain fp =                             
  do pandoc <- runWithStdlib $ parseFile fp
     return (writePlain def pandoc)       


-- |Run a Document, return the resulting Pandoc
runDocument :: DocumentM x -> IO Pandoc
runDocument d =
  do (pd,_) <- runStateT compileD nullDocument
     return pd
  where compileD = d >> compile


-- -- |Get a list of commands in the current document
-- commandNames :: DocumentM (Vector String)
-- commandNames =
--   do cmds <- fmap definedCommands get
--      return (foldMap (\accum cmd ->
--                       mappend accum
--                               (V.cons (cmdPrimary cmd)
--                                       (cmdAliases cmd)))
--                    cmds)

-- |Insert an 'Ignore' into the 'Document'
ignore :: String -> DocumentM ()
ignore = insertPart . Ignore

-- |Insert a 'Comment' into the 'Document'
comment :: String -> DocumentM ()
comment = insertPart . Comment

-- |Insert a 'DocumentPart' into the document
insertPart :: DocumentPart -> DocumentM ()
insertPart x = parts <>= [x]
