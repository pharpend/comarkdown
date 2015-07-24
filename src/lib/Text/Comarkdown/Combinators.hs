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

module Text.Comarkdown.Combinators where

import Text.Comarkdown.Parser
import Text.Comarkdown.Types

import Control.Exceptional
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Bifunctor
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.HashMap.Lazy as H
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import Text.Parsec
import Text.Pandoc

-- |Parse a ByteString into the current document.
-- 
-- The source name is required for error messages
parse :: (MonadState Document m,MonadIO m)
      => SourceName -> ByteString -> m ()
parse sn bs =
  do doc <- get
     exceptionalDocument <- liftIO $ parse' doc sn bs
     mDocument <- runExceptional exceptionalDocument
     put mDocument

-- |Parse a ByteString, given an existing document (with definitions and stuff),
-- the name of the source, and a Bytestring to parse.
parse' :: Document -> SourceName -> ByteString -> IO (Exceptional Document)
parse' doc sn bs =
  runParserT documentParser doc sn bs >>=
  return .
  \case
    Left parseError -> fail (show parseError)
    Right parts -> return (doc {docParts = mappend (docParts doc) parts})

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
  do contents <- B.readFile fp
     parse' doc fp contents

-- |Compile the current document
-- 
-- > compile = fmap toCf get >>= runExceptional . compile'
compile :: MonadState Document m => m Pandoc
compile = fmap toCf get >>= runExceptional . compile' 

-- |Attempt to compile a document into text. If it doesn't work, give back an
-- error message.
compile' :: CompilerForm
         -> Exceptional Pandoc
compile' compilerForm =
  fromEither (bimap mconcat mconcat (foldExceptional textParts))
  where textParts =
          for (cfParts compilerForm)
              (\case
                 Comment _ -> return mempty
                 Ignore txt ->
                   case readMarkdown def
                                     (T.unpack txt) of
                     Left pde -> fail (mconcat ["Pandoc error: ",show pde])
                     Right pd -> return pd
                 CommandCall cmdnom args' ->
                   case H.lookup cmdnom (cfCommands compilerForm) of
                     Just cmd -> applyTextFunction cmdnom cmd args'
                     Nothing ->
                       fail (mappend "Command not found: " (T.unpack cmdnom))
                 EnvironmentCall envnom txt args' ->
                   case H.lookup envnom (cfEnvironments compilerForm) of
                     Just env ->
                       do txtf <- env txt
                          applyTextFunction envnom txtf args'
                     Nothing ->
                       fail (mappend "Environment not found: " (T.unpack envnom)))
        for = flip fmap
        applyTextFunction = applyTextFunction' 0
        applyTextFunction' :: Int                -- Arity so far
                           -> Text               -- Function name
                           -> TextFunction       -- The function
                           -> Vector Text        -- Arguments
                           -> Exceptional Pandoc
        applyTextFunction' i funname tf txts =
          case (tf,txts !? 0) of
            -- No more arguments to send, and we have a result, send back the
            -- result
            (Result _ x,Nothing) -> fromPandoc' x
            -- More arguments to send, but have a result, fail with an arity
            -- mismatch
            (Result _ _,Just _) ->
              fail (mconcat ["Arity mismatch in function: "
                            ,T.unpack funname
                            ,". Function has an arity of "
                            ,show i
                            ," but more arguments were supplied."])
            -- No more arguments to send, but we don't have a result
            (MoreInput _ _,Nothing) ->
              fail (mconcat ["You did not send enough arguments to the function "
                            ,T.unpack funname
                            ,". Comarkdown functions have indeterminate arity, so I cannot tell you how many arguments you need to send, only that you have not sent enough."])
            -- We don't have a result, but we do have more arguments to
            -- send. Send them!
            (MoreInput _ ttntf,Just x) ->
              do newTF <- ttntf x
                 applyTextFunction' (i + 1)
                                    funname
                                    newTF
                                    (V.tail txts)
        fromPandoc'
          :: Either PandocError Pandoc -> Exceptional Pandoc
        fromPandoc' = fromEither . first show

-- |This inserts a command into the document state. If such a command already
-- exists, it will return an error message.
-- 
-- Since: 0.1.0.0
newCommand :: (MonadState Document m,ToTextFunction t)
           => CommandName
           -> [CommandName]
           -> DocString
           -> t
           -> m (Exceptional ())
newCommand prim als doc fn =
  do oldState <- get
     let newcmd =
           Command prim
                   (V.fromList als)
                   doc
                   (toTextFunction fn)
         oldcmds = definedCommands oldState
         -- Test to see if any of cmd's tokens are a token of another command
         oldTokens =
           foldl (\stuff cmd ->
                    mappend stuff
                            (V.cons (cmdPrimary cmd)
                                    (cmdAliases cmd)))
                 mempty
                 oldcmds
         errorMessages =
           foldl (\accum token' ->
                    if token' `elem` oldTokens
                       then V.snoc accum
                                   (mappend (T.unpack token')
                                            " is already in use by another command.")
                       else accum)
                 mempty
                 (V.cons (cmdPrimary newcmd)
                         (cmdAliases newcmd))
     if V.null errorMessages
        then Success <$>
             put (oldState {definedCommands = V.cons newcmd oldcmds})
        else return (Failure (mconcat ["There were errors while trying to make the command "
                                      ,T.unpack (cmdPrimary newcmd)
                                      ,". They are all listed here:"
                                      ,mconcat (V.toList (fmap (mappend "\n    ") errorMessages))]))

-- |This defines an environment in the current document state. If such a environment already
-- exists, it will return an error message.
-- 
-- Since: 0.1.0.0
newEnvironment :: (MonadState Document m,ToTextFunction t)
               => EnvironmentName
               -> [EnvironmentName]
               -> DocString
               -> (Text -> Exceptional t)
               -> m (Exceptional ())
newEnvironment prim als doc fn =
  do oldState <- get
     let newenv =
           Environment prim
                       (V.fromList als)
                       doc
                       (fmap toTextFunction . fn)
         oldenvs = definedEnvironments oldState
         -- Test to see if any of env's tokens are a token of another environment
         oldTokens =
           foldl (\stuff env ->
                    mappend stuff
                            (V.cons (envPrimary env)
                                    (envAliases env)))
                 mempty
                 oldenvs
         errorMessages =
           foldl (\accum token' ->
                    if token' `elem` oldTokens
                       then V.snoc accum
                                   (mappend (T.unpack token')
                                            " is already in use by another environment.")
                       else accum)
                 mempty
                 (V.cons (envPrimary newenv)
                         (envAliases newenv))
     if V.null errorMessages
        then Success <$>
             put (oldState {definedEnvironments = V.cons newenv oldenvs})
        else return (Failure (mconcat ["There were errors while trying to make the environment "
                                      ,T.unpack (envPrimary newenv)
                                      ,". They are all listed here:"
                                      ,mconcat (V.toList (fmap (mappend "\n    ") errorMessages))]))
