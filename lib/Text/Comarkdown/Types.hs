{-# LANGUAGE Trustworthy #-}

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
import Data.Yaml

-- |A document is many 'DocumentPart's
type Document = Vector DocumentPart

-- |There are a few possible objects:
-- 
-- 1. ordinary markdown text, represented as an 'MDPart',
-- 2. the definition of a 'Defn',
-- 3. the application of a 'Defn',
-- 4. a YAML block containing some metadata,
-- 5. importing data from another file,
-- 6. embedding content from another file.
data DocumentPart
  = Apply Defn
  | Define Defn
  | Embed FilePath
  | Import FilePath
  | Markdown MDPart
  | Metadata Value
  deriving (Eq, Show)
  
-- |A definition. This can be
-- 
-- 1. a function,
-- 2. a mixin, or
-- 3. an environment.
data Defn
  = Environment
  | Function
  | Mixin
  deriving (Eq, Show)

-- |A sum type for the markdown parts
data MDPart
  = Bold Text
  |
    -- |These correspond to @\<h1\>@, @\<h2\>@, etc in HTML and to
    -- @\\chapter@, @\\section@, @\\subsection@, @\\subsubsection@, and
    -- @\\paragraph@ in LaTeX (respectively).
    Header1 Text
  | Header2 Text
  | Header3 Text
  | Header4 Text
  | Header5 Text
  | Italic Text
  |
    -- |@\<ol\>@ in HTML, or @enumerate@ in LaTeX
    ListOrdered (Vector Document)
  |
    -- |@\<ul\>@ in HTML, or @itemize@ in LaTeX
    ListUnordered (Vector Document)
  |
    -- |@\<dl\>@ in HTML, or @description@ in LaTeX
    ListKeyValue (Map Text Document)
  | Literal Text
  | Paragraph Text
  deriving (Eq,Show)
