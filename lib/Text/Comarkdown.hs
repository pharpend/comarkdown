-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or (at
-- your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- | 
-- Module      : Text.Comarkdown
-- Description : The Comarkdown library
-- Copyright   : Copyright 2015 Peter Harpending.
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : portable

module Text.Comarkdown where

import Data.Map (Map)
import Data.Text (Text)
import Data.Vector (Vector)

-- |A document is a list of 'DocumentPart's
type Document = Vector DocumentPart

-- |A sum type for the document parts
data DocumentPart
  =
    -- |These correspond to @\<h1\>@, @\<h2\>@, etc in HTML and to
    -- @\\chapter@, @\\section@, @\\subsection@, @\\subsubsection@, and
    -- @\\paragraph@ in LaTeX (respectively).
    Header1 Text
  | Header2 Text
  | Header3 Text
  | Header4 Text
  | Header5 Text
  |
    -- |@\<ol\>@ in HTML, or @enumerate@ in LaTeX
    ListOrdered (Vector Document)
  |
    -- |@\<ul\>@ in HTML, or @itemize@ in LaTeX
    ListUnordered (Vector Document)
  |
    -- |@\<dl\>@ in HTML, or @description@ in LaTeX
    ListKeyValue (Map Text Document)
  | Paragraph Text
  deriving (Eq,Show)
