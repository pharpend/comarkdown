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
import Data.Vector (Vector)
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

-- |Attempt to take the current document and make a 'Pandoc' from it. There are
-- a number of errors that could occur. For a version that catches errors, use
-- 'compile\''.
-- 
-- > compile = fmap toCf get >>= runExceptional . compile'
compile :: MonadState Document m => m Pandoc
compile = fmap toCf get >>= runExceptional . compile' 

-- |Attempt to take the given 'CompilerForm' and produce a 'Pandoc' from it.
-- 
-- There are a number of errors that can occur:
-- 
-- * Pandoc could produce some sort of parser error.
-- 
-- * There could be a call to a command or environment that doesn't exist, in
-- which case an error is thrown.
-- 
-- * There could be an arity error in a command/environment call. This would
-- include a required argument that was not supplied.
-- 
-- * There could be a value error with the command call
compile' :: CompilerForm
         -> Exceptional Pandoc
compile' compilerForm =
  -- Concatenate the error messages or Pandocs, then produce an 'Exceptional'
  -- value.
  fromEither withErrorMessages
  where withErrorMessages :: Either String Pandoc
        withErrorMessages = bimap mconcat mconcat (foldExceptional textParts)
        textParts :: Vector (Exceptional Pandoc)
        textParts =
          -- Loop through the parts that the parser found, perform case analysis
          -- on them.
          for (cfParts compilerForm) $
          \case
            -- If it's a comment, we don't want any output, so produce
            -- 'mempty'
            Comment _ -> return mempty
            -- If it's text to be inserted literally (i.e. not macro-expanded
            -- or whatever), then just send it straight to Pandoc
            Ignore txt ->
              fromPandoc'
                (readMarkdown def
                              (T.unpack txt))
            -- If it's a command call...
            CommandCall cmdnom mkvs ->
              -- Lookup the command to make sure it exists...
              case H.lookup cmdnom (cfCommands compilerForm) of
                -- If the command doesn't exist, then throw an error
                Nothing ->
                  fail (mappend "Command not found: " (T.unpack cmdnom))
                -- If it does exist, then attempt to run the command call
                Just cmd ->
                  do argumentMap <- mkArgMap mkvs (cmdArguments cmd)
                     resultingText <- cmdFunction cmd argumentMap
                     fromPandoc'
                       (readMarkdown def
                                     (T.unpack resultingText))
            -- We're essentially doing the same thing with the environment call,
            -- except the semantics are slightly different, because the minimum
            -- arity is 1.
            EnvironmentCall envnom txt mkvs ->
              case H.lookup envnom (cfEnvironments compilerForm) of
                Nothing ->
                  fail (mappend "Environment not found: " (T.unpack envnom))
                Just env ->
                  do argumentMap <- mkArgMap mkvs (envArguments env)
                     resultingText <- envFunction env txt argumentMap
                     fromPandoc'
                       (readMarkdown def
                                     (T.unpack resultingText))
        for = flip fmap
        fromPandoc' :: Either PandocError Pandoc -> Exceptional Pandoc
        fromPandoc' = fromEither . first show

-- |This creates a command. This will error out if the command already exists.
newCommand :: MonadState Document m
           => CommandName
           -> [CommandName]
           -> DocString
           -> [Argument]
           -> TextFunction
           -> m ()
newCommand primaryName alternateNames commandDocumentation commandArguments commandFunction =
  do oldState <- get
     let newcmd =
           Command primaryName
                   (V.fromList alternateNames)
                   commandDocumentation
                   (V.fromList commandArguments)
                   commandFunction
         oldcmds = definedCommands oldState
         -- Form a uniform list of all of the existing aliases and primary
         -- command names.
         oldTokens =
           foldl (\stuff cmd ->
                    mappend stuff
                            (V.cons (cmdPrimary cmd)
                                    (cmdAliases cmd)))
                 mempty
                 oldcmds
         -- Check to make sure neither the primary command name or the aliases
         -- are already in use. This collects the error messages.
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
     -- If we don't have any error messages, then continue on
     if V.null errorMessages
        then put (oldState {definedCommands = V.cons newcmd oldcmds})
        -- Otherwise, fail
        else fail (mconcat ["There were errors while trying to make the command "
                           ,T.unpack (cmdPrimary newcmd)
                           ,". They are all listed here:"
                           ,mconcat (V.toList (fmap (mappend "\n    ") errorMessages))])

-- |This creates a environment. This will error out if the environment already exists.
newEnvironment :: MonadState Document m
               => EnvironmentName
               -> [EnvironmentName]
               -> DocString
               -> [Argument]
               -> (Text -> TextFunction)
               -> m ()
newEnvironment primaryName alternateNames environmentDocumentation environmentArguments environmentFunction =
  do oldState <- get
     let newenv =
           Environment primaryName
                       (V.fromList alternateNames)
                       environmentDocumentation
                       (V.fromList environmentArguments)
                       environmentFunction
         oldenvs = definedEnvironments oldState
         -- Form a uniform list of all of the existing aliases and primary
         -- environment names.
         oldTokens =
           foldl (\stuff env ->
                    mappend stuff
                            (V.cons (envPrimary env)
                                    (envAliases env)))
                 mempty
                 oldenvs
         -- Check to make sure neither the primary environment name or the aliases
         -- are already in use. This collects the error messages.
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
     -- If we don't have any error messages, then continue on
     if V.null errorMessages
        then put (oldState {definedEnvironments = V.cons newenv oldenvs})
        else
             -- Otherwise, fail
             fail
               (mconcat ["There were errors while trying to make the environment "
                        ,T.unpack (envPrimary newenv)
                        ,". They are all listed here:"
                        ,mconcat (V.toList (fmap (mappend "\n    ") errorMessages))])
