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

-- import Control.Exceptional
-- import Data.Default
-- import Data.Text (Text)
-- import qualified Data.Text as T
-- import Text.Comarkdown.Combinators
-- import Text.Comarkdown.Types

-- infixl 5 <+>
-- (<+>) :: Monoid m => m -> m -> m
-- (<+>) = mappend

-- stdlib :: DocumentM (Exceptional ())
-- stdlib =
--   do newCommand "bold-face"
--                 ["bf","xtb"]
--                 "Put something in bold-face"
--                 (pck "Text to italicize" (\x -> result $ "**" <+> x <+> "**"))
--      newCommand "italic"
--                 ["it","xti"]
--                 "Put something in italic"
--                 (pck "Text to italicize" (\x -> result $ "*" <+> x <+> "*"))
--      newEnvironment
--        "ignore"
--        []
--        "Ignore this stuff"
--        (\x -> Success (pck "Text to ignore (or rather, insert literally." x))
--   where result
--           :: Text -> (DocString,Either PandocError Pandoc)
--         result x = ("result",md x)
--         pck :: DocString -> x -> (DocString, x)
--         pck x y = (x,y)
--         md = readMarkdown def . T.unpack
