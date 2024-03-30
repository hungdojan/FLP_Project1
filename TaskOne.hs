-- This module contains function and utilities used to solve the first task of
-- the assignment.
--
-- This source code serves as the submission to the first assignment
-- of class FLP at FIT, BUT 2023/24
--
-- @author  Hung Do
-- @date    04/06/2024
-- @file    TaskOne.hs
module TaskOne where

import           Control.Monad (forM_)
import           Data.Maybe
import           Parser
import           System.IO
import           Tree

-- | Create a `Node` from the given tokens `[String]`.
-- Takes three parameters:
--    `Handle`   - A file handle used by `loadTreeFromFile`.
--    `[String]` - A collection of parsed tokens.
--    `Int`      - A node height used by `loadTreeFromFile`.
-- Returns a `Just Node` constructor if a tree was successfully reconstructed,
-- otherwise `Nothing`.
createNodeFromData :: Handle -> [String] -> Int -> IO (Maybe (Tree Int Float String))
createNodeFromData _ [] _ = return Nothing
createNodeFromData _ [_] _ = return Nothing
createNodeFromData _ [_, _] _ = return Nothing
createNodeFromData h (_ : fData : th : _) n = do
  -- generate child nodes
  left <- loadTreeFromFile h (n + 1)
  right <- loadTreeFromFile h (n + 1)
  -- check their validity
  if isNothing left || isNothing right
    then return Nothing
    else do
      return $
        Just $
          Node (read fData :: Int) (read th :: Float) (fromJust left) (fromJust right)

-- | Create a `Leaf` from the given tokens `[String]`.
-- Takes three parameters:
--    `Handle`   - A file handle used by `loadTreeFromFile`.
--    `[String]` - A collection of parsed tokens.
--    `Int`      - A node height used by `loadTreeFromFile`.
-- Returns a `Just Leaf` constructor if a tree was successfully reconstructed,
-- otherwise `Nothing`.
createLeafFromData :: [String] -> Maybe (Tree Int Float String)
createLeafFromData []                  = Nothing
createLeafFromData [_]                 = Nothing
createLeafFromData (_ : className : _) = Just $ Leaf className

loadTreeFromFile :: Handle -> Int -> IO (Maybe (Tree Int Float String))
loadTreeFromFile h lvl = do
  eof <- hIsEOF h
  if eof
    then return Nothing
    else do
      -- is not EOF
      line <- hGetLine h
      let (lineWithoutIndent, indentSize) = removeIndent line
      (valid, _data@(tType : _)) <- parseLine lineWithoutIndent
      -- check if input line has a valid format and the data is well indented
      if valid && lvl == indentSize `div` 2
        then case tType of
          "Leaf" -> return $ createLeafFromData _data
          "Node" -> createNodeFromData h _data lvl
          _      -> return Nothing
        else do
          return Nothing -- error "Invalid input file"

-- | Task 1 logic.
taskOne :: FilePath -> FilePath -> IO ()
taskOne fInput dInput = do
  tree <-
    withFile
      fInput
      ReadMode
      ( \handle -> do
          loadTreeFromFile handle 0
      )

  if isJust tree
    then do
      dataInputContent <- readFile dInput
      forM_ (lines dataInputContent) $ \x -> do
        let stripValues = map ((\_x -> (read _x :: Float)) . strip (== ' ')) (split x ',')
        putStrLn $ getClass stripValues $ fromJust tree
    else putStrLn "I was not able to reconstructed the tree from the file."
