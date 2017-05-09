module CPM.Diff.CurryComments
  ( readComments
  , SourceLine
  , getFuncComment
  ) where

import Char
import List (isSuffixOf)

-- This is adapted from the currydoc source code.

--- Reads the pragma comments from a Curry program.
--- The first component of the result
--- is the comment for the module definition. The second component is a map
--- from different source line types to pragma comments on that source line.
readComments :: String -> IO (String, [(SourceLine, String)])
readComments filename = do
  prog <- readFile filename
  return (groupLines . filter (/= OtherLine) . map classifyLine . lines $ prog)

data SourceLine = PragmaCmt String
                | ModDef
                | DataDef String
                | FuncDef String
                | OtherLine
 deriving Eq

classifyLine :: String -> SourceLine
classifyLine line
  | take 3 line == "{-#" = PragmaCmt (drop 3 line) -- #-}
  | take 7 line == "module " = ModDef
  | take 7 line == "import " = ModDef
  | otherwise = if null id1
                  then OtherLine
                  else if id1 == "data" || id1 == "type" || id1 == "newtype"
                        then DataDef (getDatatypeName line)
                        else if "'default" `isSuffixOf` id1
                              then OtherLine 
                              else FuncDef id1
   where
    id1 = getFirstId line
    getDatatypeName = takeWhile isIdChar . dropWhile (== ' ') . dropWhile isIdChar

getFirstId :: String -> String
getFirstId [] = ""
getFirstId (c:cs)
  | isAlpha c = takeWhile isIdChar (c:cs)
  | c == '('  = let bracketId = takeWhile (/= ')') cs
                 in if all (`elem` infixIDs) bracketId
                    then bracketId
                    else ""
  | otherwise = ""

isIdChar :: Char -> Bool
isIdChar c = isAlphaNum c || c == '_' || c == '\''

infixIDs :: String
infixIDs =  "~!@#$%^&*+-=<>?./|\\:"

groupLines :: [SourceLine] -> (String, [(SourceLine, String)])
groupLines sls = 
  let (modCmts, progCmts) = break (== ModDef) sls
   in if progCmts == []
      then ("", groupProgLines sls)
      else (concatMap getComment modCmts,
            groupProgLines (filter (/= ModDef) (tail progCmts)))
 where
  getComment src = case src of
    PragmaCmt cmt -> cmt ++ "\n"
    _             -> ""

groupProgLines :: [SourceLine] -> [(SourceLine, String)]
groupProgLines [] = []
groupProgLines (PragmaCmt cmt : sls) = groupComment cmt sls
groupProgLines (FuncDef f     : sls) = (FuncDef f, "") : skipFuncDefs f sls
groupProgLines (DataDef d     : sls) = (DataDef d, "") : skipDataDefs d sls
groupProgLines (ModDef        : sls) = groupProgLines sls
groupProgLines (OtherLine     : sls) = groupProgLines sls

groupComment :: String -> [SourceLine] -> [(SourceLine, String)]
groupComment _ [] = []
groupComment cmt (PragmaCmt cmt1 : sls) = groupComment (cmt ++ "\n" ++ cmt1) sls
groupComment cmt (FuncDef f      : sls) = (FuncDef f, cmt) : skipFuncDefs f sls
groupComment cmt (DataDef d      : sls) = (DataDef d, cmt) : skipDataDefs d sls
groupComment cmt (ModDef         : sls) = groupComment cmt sls
groupComment cmt (OtherLine      : sls) = groupComment cmt sls

skipFuncDefs :: String -> [SourceLine] -> [(SourceLine, String)]
skipFuncDefs _ [] = []
skipFuncDefs _ (PragmaCmt cmt : sls) = groupProgLines (PragmaCmt cmt : sls)
skipFuncDefs _ (DataDef d     : sls) = groupProgLines (DataDef d   : sls)
skipFuncDefs f (FuncDef f1    : sls) =
  if f == f1 then skipFuncDefs f sls
             else groupProgLines (FuncDef f1 : sls)
skipFuncDefs f (ModDef        : sls) = skipFuncDefs f sls
skipFuncDefs f (OtherLine     : sls) = skipFuncDefs f sls

skipDataDefs :: String -> [SourceLine] -> [(SourceLine, String)]
skipDataDefs _ [] = []
skipDataDefs _ (PragmaCmt cmt : sls) = groupProgLines (PragmaCmt cmt : sls)
skipDataDefs _ (FuncDef f     : sls) = groupProgLines (FuncDef f   : sls)
skipDataDefs d (DataDef d1    : sls) = 
  if d == d1 then skipDataDefs d sls
             else groupProgLines (DataDef d1 : sls)
skipDataDefs d (ModDef        : sls) = skipDataDefs d sls
skipDataDefs d (OtherLine     : sls) = skipDataDefs d sls

--- Get the pragma comments for a function from a map from source lines
--- to comments.
getFuncComment :: String -> [(SourceLine, String)] -> String
getFuncComment _ [] = ""
getFuncComment fname ((def, cmt):fdcmts) = case def of
  FuncDef f -> if fname == f then cmt else getFuncComment fname fdcmts
  _         -> getFuncComment fname fdcmts
