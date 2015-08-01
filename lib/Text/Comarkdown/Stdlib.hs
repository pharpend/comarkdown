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
-- Module      : Text.Comarkdown.Stdlib
-- Description : The comarkdown standard library
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module Text.Comarkdown.Stdlib where

import Text.Comarkdown.Combinators
import Text.Comarkdown.Types

stdlib :: DocumentM ()
stdlib =
  do newCommand "bold-face"
                ["embolden","Emphasize","bf","xtb"]
                "Put something in bold-face"
                [Argument "txt" "The text to embolden" Nothing]
                (\v -> md (mconcat ["**",v ! "txt","**"]))
     newCommand "italic"
                ["italicize","emphasize","it","xti"]
                "Put something in italic"
                [Argument "txt" "The text to italicize" Nothing]
                (\v -> md (mconcat ["*",v ! "txt","*"]))
     newEnvironment "ignore"
                    []
                    "Interpret everything in the environment literally. Note that this just bypasses any comarkdown macros in the text. It is still interpreted as normal markdown."
                    []
                    (\txt _ -> md txt)
