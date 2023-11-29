--- Some auxiliary operations that might fit better into system libraries.

module CPM.Helpers ( stripSpaces, stripEnclosing, askYesNo ) where

import Data.Char ( isSpace, toLower )
import Data.List ( init, isPrefixOf, last )
import System.IO ( hFlush, stdout )

--- Strips leading and tailing spaces:
stripSpaces :: String -> String
stripSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace

--- Strips a leading and tailing character if they are present.
stripEnclosing :: Char -> Char -> String -> String
stripEnclosing lc tc s = case s of
  c1:cs@(_:_) | c1 == lc && last cs == tc -> init cs
  _                                       -> s

-- Ask a question and return the answer which must be empty, `yes`, or `no`.
askYesNo :: String -> IO String
askYesNo question = do
  putStr question
  hFlush stdout
  answer <- fmap (map toLower) getLine
  if null answer
    then return answer
    else if answer `isPrefixOf` "yes"
           then return "yes"
           else if answer `isPrefixOf` "no"
                  then return "no"
                  else askYesNo question -- again

