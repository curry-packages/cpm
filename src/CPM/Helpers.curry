--- Some auxiliary operations that might fit better into system libraries.

module CPM.Helpers ( stripSpaces ) where

import Data.Char ( isSpace )

-- Strips leading and tailing spaces:
stripSpaces :: String -> String
stripSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace
