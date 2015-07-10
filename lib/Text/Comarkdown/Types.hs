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
-- Module      : Text.Comarkdown.Types
-- Description : Types for the Comarkdown library
-- Copyright   : Copyright 2015 Peter Harpending.
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : portable

module Text.Comarkdown.Types where

import Data.Map (Map)
import Data.Text.Lazy (Text)
import Data.Vector (Vector)

type Document = Vector ComarkdownPart

-- |The type to which files are marshaled.
data ComarkdownPart
    -- |Definition of a macro
  = Define DefMacro
    -- |Top-level application of a macro
  | BareApplyMacro ApplyMacro
    -- |Importing a file.
    -- 
    -- Note that importing only imports definitions and their documentation.
  | Import FilePath
    -- |Input a file. This is equivalent to cut & pasting the entire file into the document
  | Input FilePath
    -- |A comment
  | Comment Text
    -- |Markdown!
  | Header Int (Vector HeaderPart)
  | Paragraph (Vector ParagraphPart)
  | IndentCode Text
  | FencedCode
      -- |The language
      Text
      -- |The code itself
      Text
  deriving (Eq,Show)

-- |Potential macros
data DefMacro
    -- |Equivalent to a @\\newcommand@ in LaTeX.
  = DefCommand 
      -- |The name of the command
      Text
      -- |A list of arguments with optional default values
      (Map Text (Maybe Text))
      -- |What to do with all of this information
      Text
    -- |Equivalent to a @\\newenvironment@ in LaTeX.
  | DefEnvironment 
      -- |Name of the environment
      Text
      -- |A name for the primary data
      Text
      -- |A list of variable names with optional default values
      (Map Text (Maybe Text))
      -- |What to do with all of this information
      Text
  deriving (Eq,Show)

-- |How to apply a macro
data ApplyMacro
    -- |Apply a command
  = ApCommand
      -- |Name of the command
      Text
      -- |Anonymous arguments
      (Vector Text)
      -- |Kwargs
      (Map Text Text)
    -- |Apply an environment
  | ApEnvironment
      -- |Name of the environment
      Text
      -- |Anonymous arguments
      (Vector Text)
      -- |Kwargs
      (Map Text Text)
      -- |Main body
      Text
  deriving (Eq,Show)


-- |Header parts
data HeaderPart
  = HdrPlainWord Text
  | HdrBoldWord Text
  | HdrItalicWord Text
  | HdrMonospaceBlob Text
  | HdrApplyMacro ApplyMacro
  deriving (Eq,Show)

-- |Components to a paragraph.

data ParagraphPart
  = ParPlainWord Text
  | ParBoldWord Text
  | ParItalicWord Text
    -- |Everything here is taken literally, except newlines, which are ignored.
  | ParMonospaceBlob Text
  | ParApplyMacro ApplyMacro
  | Link 
      -- |The visual thing
      (Vector ParagraphPart)
      -- |The actual URL
      (Either 
         -- |A canonical URL (i.e. in parens)
         Text
         -- |A reference (i.e. in square brackets)
         Text)
  | Footnote Text
  | OrderedList 
      -- |A list of items
      (Vector
         -- |Each of which is a list of paragraphs
         (Vector
            -- |Each of which is a list of 'ParagraphPart's
            (Vector ParagraphPart)))
  | UnorderedList 
      -- |A list of items
      (Vector
         -- |Each of which is a list of paragraphs
         (Vector
            -- |Each of which is a list of 'ParagraphPart's
            (Vector ParagraphPart)))
  deriving (Eq,Show)
