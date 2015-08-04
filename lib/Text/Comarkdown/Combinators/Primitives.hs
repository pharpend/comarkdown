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
-- Module      : Text.Comarkdown.Combinators.Primitives
-- Description : Primitive combinators for comarkdown
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable
-- 
-- The distinction is rather arbitrary. It just so happens that the dependency
-- tree looks like this:
-- 
-- > Text.Comarkdown.Combinators.Primitives
-- > |\
-- > | Text.Comarkdown.Stdlib
-- >  \|
-- >   Text.Comarkdown.Combinators
-- 
-- So the stuff that is needed by *both* modules is here.

module Text.Comarkdown.Combinators.Primitives where

import Text.Comarkdown.Types

import Control.Exceptional
import Control.Lens
import Control.Monad.State
import qualified Data.HashMap.Lazy as H
import Data.Traversable (for)

-- *** Lenses
makeLensesFor
  [("definedCommands","commands")
  ,("definedEnvironments","environments")
  ,("docParts","parts")]
  ''Document
 
-- |Attempt to take the current document and make a 'Pandoc' from it. There are
-- a number of errors that could occur. 
compile :: DocumentM Pandoc
compile =
  do cf <- toCf <$> get
     parts' <-
       for (cfParts cf) $
       \case
         -- If it's a comment, we don't want any output, so produce
         -- 'mempty'
         Comment _ -> return mempty
         -- If it's text to be inserted literally (i.e. not macro-expanded
         -- or whatever), then just send it straight to Pandoc
         Ignore txt ->
           fromPandoc'
             (readMarkdown (cfOptions cf) txt)
         -- If it's a command call...
         CommandCall cmdnom mkvs ->
           -- Lookup the command to make sure it exists...
           case H.lookup cmdnom (cfCommands cf) of
             -- If the command doesn't exist, then throw an error
             Nothing -> fail (mappend "Command not found: " cmdnom)
             -- If it does exist, then attempt to run the command call
             Just cmd ->
               do argumentMap <-
                    runExceptional (mkArgMap mkvs (cmdArguments cmd))
                  cmdFunction cmd argumentMap
         -- We're essentially doing the same thing with the environment call,
         -- except the semantics are slightly different, because the minimum
         -- arity is 1.
         EnvironmentCall envnom txt mkvs ->
           case H.lookup envnom (cfEnvironments cf) of
             Nothing -> fail (mappend "Environment not found: " envnom)
             Just env ->
               do argumentMap <-
                    runExceptional (mkArgMap mkvs (envArguments env))
                  envFunction env txt argumentMap
     return (foldl mappend mempty parts')
          

-- |Internal function to switch from pandoc's error type into the DocumentM
-- type.
fromPandoc' :: Either PandocError Pandoc -> DocumentM Pandoc
fromPandoc' =
  \case
    Left err -> fail (show err)
    Right x -> return x

-- |Compile pure markdown text into a pandoc
md :: String -> DocumentM Pandoc
md s =
  do opts <- fmap docOptions get
     fromPandoc' (readMarkdown opts s)

-- |This creates a command. This will error out if the command already exists.
newCommand :: MonadState Document m
           => CommandName
           -> [CommandName]
           -> DocString
           -> [Argument]
           -> StringFunction
           -> m ()
newCommand primaryName alternateNames commandDocumentation commandArguments commandFunction =
  do oldState <- get
     let newcmd =
           Command primaryName alternateNames commandDocumentation commandArguments commandFunction
         oldcmds = definedCommands oldState
         -- Form a uniform list of all of the existing aliases and primary
         -- command names.
         oldTokens = foldMap (\cmd -> cmdPrimary cmd : cmdAliases cmd) oldcmds
         -- Check to make sure neither the primary command name or the aliases
         -- are already in use. This collects the error messages.
         errorMessages =
           foldMap (\token' ->
                      if token' `elem` oldTokens
                         then [mappend token' " is already in use by another command."]
                         else mempty)
                   (cmdPrimary newcmd : cmdAliases newcmd)
     -- If we don't have any error messages, then continue on
     if null errorMessages
        then commands %= (newcmd :)
        else -- Otherwise, fail
             fail
               (mconcat ["There were errors while trying to make the command "
                        ,cmdPrimary newcmd
                        ,". They are all listed here:"
                        ,mconcat (fmap (mappend "\n    ") errorMessages)])

-- |This creates a environment. This will error out if the environment already exists.
newEnvironment :: MonadState Document m
               => EnvironmentName
               -> [EnvironmentName]
               -> DocString
               -> [Argument]
               -> (String -> StringFunction)
               -> m ()
newEnvironment primaryName alternateNames environmentDocumentation environmentArguments environmentFunction =
  do oldState <- get
     let newenv =
           Environment primaryName
                       alternateNames
                       environmentDocumentation
                       environmentArguments
                       environmentFunction
         oldenvs = definedEnvironments oldState
         -- Form a uniform list of all of the existing aliases and primary
         -- environment names.
         oldTokens =
           foldMap (\env -> (envPrimary env : envAliases env)) oldenvs
         -- Check to make sure neither the primary environment name or the aliases
         -- are already in use. This collects the error messages.
         errorMessages =
           foldMap (\token' ->
                      if token' `elem` oldTokens
                         then [mappend token' " is already in use by another environment."]
                         else mempty)
                   (envPrimary newenv : envAliases newenv)
     -- If we don't have any error messages, then continue on
     if null errorMessages
        then environments %= (newenv :)
        else -- Otherwise, fail
             fail
               (mconcat ["There were errors while trying to make the environment "
                        ,envPrimary newenv
                        ,". They are all listed here:"
                        ,mconcat (fmap (mappend "\n    ") errorMessages)])
