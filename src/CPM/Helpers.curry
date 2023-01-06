--- Some auxiliary operations that might fit better into system libraries.

module CPM.Helpers ( stripSpaces, stripEnclosing ) where

import Data.Char ( isSpace )
import Data.List ( init, last )

--- Strips leading and tailing spaces:
stripSpaces :: String -> String
stripSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace

--- Strips a leading and tailing character if they are present.
stripEnclosing :: Char -> Char -> String -> String
stripEnclosing lc tc s = case s of
  c1:cs@(_:_) | c1 == lc && last cs == tc -> init cs
  _                                       -> s
