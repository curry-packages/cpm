--- Some auxiliary operations that might fit better into system libraries.

module CPM.Helpers ( strip ) where

import Char ( isSpace )

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

