-- This module contains utilities used for parsing an input file containing
-- a Tree structure.
--
-- This source code serves as the submission to the first assignment
-- of class FLP at FIT, BUT 2023/24
--
-- @author  Hung Do
-- @date    04/03/2024
-- @file    Parser.hs
module Parser (parseLine, strip, split, isInt, isFloat, removeIndent) where

import           Data.Char (isDigit, isSpace)
import           Data.List (dropWhileEnd, isInfixOf)

type ParseResult a b = (Bool, a, b)

data State
  = TreeType
  | Delim
  | Data
  deriving (Eq)

-- ==== START OF UTILS =======

-- inspired by a function from stack overflow
-- https://stackoverflow.com/a/30029670
isInt :: String -> Bool
isInt ""       = False
isInt ('-':xs) = dropWhile isDigit xs == ""
isInt str      = dropWhile isDigit str == ""

isFloat :: String -> Bool
isFloat "" = False
isFloat "." = False
isFloat ('-':xs) = isFloat xs
isFloat xs =
  case dropWhile isDigit xs of
    ""         -> True
    ('.' : ys) -> isDecimal ys
    _          -> False
  where
    isDecimal ys = case dropWhile isDigit ys of
                  ""                -> True
                  ('e' : '-' : yss) -> all isDigit yss
                  ('e' : yss)       -> all isDigit yss
                  _                 -> False

-- | Convert a nested pair in a pair to a triple.
nestedPairToTriples :: (a, (b, c)) -> (a, b, c)
nestedPairToTriples (x, (y, z)) = (x, y, z)

-- | Remove specific `Char` from both sides of the given `String`.
strip :: (Char -> Bool) -> String -> String
strip f = dropWhile f . dropWhileEnd f

-- | Split `String` with the given `Char`.
split :: String -> Char -> [String]
split "" _ = []
split str delim = _split str "" []
    where _split ""  tmpStr l = l ++ [tmpStr]
          _split (x:xs) tmpStr l
                | x == delim = _split xs "" (l ++ [tmpStr])
                | otherwise  = _split xs (tmpStr ++ [x]) l

-- | Function calculates and removes spaces from the beginning of the string.
-- Returns a pair of `String` and `Int` where:
--      `String` - The processed string.
--      `Int`    - Number of spaces (indentation size) removed.
removeIndent :: String -> (String, Int)
removeIndent "" = ("", 0)
removeIndent str@(x : xs)
  | x /= ' ' = (str, 0)
  | otherwise = getIndents xs 1
  where
    getIndents "" n = ("", n)
    getIndents _str@(_x : _xs) n
      | not (isSpace _x) = (_str, n)
      | otherwise = getIndents _xs (n + 1)

-- ====== END OF UNILS ========

-- | Separate a given label from the given string. The given string must
-- start with the given label. The function iterates both string from the
-- beginning until the characters don't match anymore. Then it returns the
-- part that matches and the rest of the string.
-- Returns a pair of `String` and `String` where
--      `String` - Extracted label.
--      `String` - Rest of the string.
separateSame :: String -> String -> (String, String)
separateSame "" _ = ("", "")
separateSame str "" = ("", str)
separateSame orig@(x : xs) (y : ys)
  | x /= y = ("", orig) -- given string doesn't match given label
  | otherwise = parseLbl [x] xs ys
  where
    -- recursively iterate through the string and extract the label from the string
    parseLbl _x "" _ = (_x, "")
    parseLbl _x _xs "" = (_x, _xs)
    parseLbl _x _xs@(h1 : t1) _ys@(h2 : t2)
      | h1 == h2 = parseLbl (_x ++ [h1]) t1 t2
      | otherwise = (_x, _xs)

-- | Check if the given `String` starts with
-- a valid `Tree` constructor label (defined in Tree module).
-- If so, function will separate the Tree constructor from the string.
--
-- Returns a tuple of `Bool`, `String` and `String` where
--      `Bool`   - The given string has a correct Tree constructor.
--      `String` - Separated constructor name.
--      `String` - Rest of the string.
getTreeType :: String -> ParseResult String String
getTreeType "" = (False, "", "")
getTreeType str
  | "Node" `isInfixOf` str = nestedPairToTriples (True, separateSame str "Node")
  | "Leaf" `isInfixOf` str = nestedPairToTriples (True, separateSame str "Leaf")
  | otherwise = (False, "", str)

-- | Check if the given `String` starts with a delimiter separating Tree type from
-- its data (defined as `: `).
-- If so, function will separate it from the rest of the string.
--
-- Returns a tuple of `Bool`, `String` and `String` where
--      `Bool`   - The given string has a correct Tree constructor.
--      `String` - Separated delimiter (`: `).
--      `String` - Rest of the string.
getDelim :: String -> ParseResult String String
getDelim "" = (False, "", "")
getDelim str
  | ": " `isInfixOf` str = nestedPairToTriples (True, separateSame str ": ")
  | otherwise = (False, "", str)

-- | Check if the given `String` contains valid `Node` data.
-- `Node` expects two parameters:
--      - `Int` representing node index
--      - `Float` representing the threshold
-- If so, function will separate it from the rest of the string.
--
-- Returns a tuple of `Bool`, `[String]` and `String` where
--      `Bool`     - The given string has a correct data format.
--      `[String]` - Extracted `Node` data.
--      `String`   - Rest of the string.
getNodeData :: String -> ParseResult [String] String
getNodeData str = _parseData $ split str ','
  where
    _parseData (x : xs : xss)
      | isInt x && isFloat (strip isSpace xs) = (True, [x, xs], concat xss)
      | otherwise = (False, [""], str)
    _parseData _ = (False, [""], str)

-- | Check if the given `String` contains valid `Leaf` data.
-- `Leaf` has only one parameter: name of the class.
-- If it's valid, function will separate it from the rest of the string.
--
-- Returns a tuple of `Bool`, `String` and `String` where
--      `Bool`   - The given string has a correct data format.
--      `String` - Extracted `Leaf` data.
--      `String` - Rest of the string.
getLeafData :: String -> ParseResult String String
getLeafData str = (True, str, "")

-- | Parsing data by states. The expected from of the parser is following:
--  Indent -> TreeType -> Delim -> Data
--    "  "     "Node"     ": "    "1 1.0"
-- Returns a tuple of `Bool` and `[String]` where
--      `Bool`     - The given string has a correct data.
--      `[String]` - List of tokens that where extracted by the parser.
parseByState :: State -> String -> [String] -> IO (Bool, [String])
parseByState state str l
  | state == TreeType = do
      let (res, parseData, restData) = getTreeType str
      if not res
        then do
          return (False, l ++ [str])
        else do
          parseByState Delim restData (l ++ [parseData])
  | state == Delim = do
      let (res, _, restData) = getDelim str
      if not res
        then do
          return (False, l ++ [str])
        else do
          parseByState Data restData l
  | state == Data && head l == "Node" = do
      let (res, parseData, _) = getNodeData str
      if not res
        then do
          return (False, l ++ [str])
        else do
          return (True, l ++ parseData)
  | state == Data && head l == "Leaf" = do
      let (res, parseData, _) = getLeafData str
      if not res
        then do
          return (False, l ++ [str])
        else do
          return (True, l ++ [parseData])
  | otherwise = error "Cannot parse"

-- | Function that does Tree parsing for a line.
-- Returns a tuple of `Bool` and `[String]` where
--      `Bool`     - The given string has a correct data.
--      `[String]` - List of tokens that where extracted by the parser.
parseLine :: String -> IO (Bool, [String])
parseLine str = do parseByState TreeType str []
