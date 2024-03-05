-- This module contains utilities used for processing program arguments
-- and the main function.
--
-- @author  Hung Do
-- @date    04/03/2024
-- @file    main.hs

module Main (main) where

import           Control.Monad      (forM_)
import           Data.Maybe
import           Parser             (parseLine, split, strip)
import           System.Environment
import           System.IO          (Handle, IOMode (ReadMode), hGetLine,
                                     hIsEOF, withFile)
import           Tree

data Args = Args
  { taskType   :: String,
    firstInput :: FilePath,
    dataInput  :: FilePath
  }
  deriving (Show)

-- load arguments
processArgs :: [String] -> IO Args
processArgs (tT@"-1" : fI : dI : _) = return (Args tT fI dI)
processArgs (tT@"-2" : fI : _)      = return (Args tT fI "")
processArgs _                       = error "Wrong input"

getIndentation :: (Char -> Bool) -> String -> (String, Int)
getIndentation f str = _calculate f str 0
  where
    _calculate _ "" n = ("", n)
    _calculate _f _str@(x : xs) n
      | _f x = _calculate _f xs (n + 1)
      | otherwise = (_str, n)

loadTree :: Handle -> IO (Maybe (Tree Int Float String))
loadTree handle = do
  eof <- hIsEOF handle
  if eof
    then return Nothing
    else do
      line <- hGetLine handle
      (valid, _data) <- parseLine line
      if valid
        then do
          -- TODO:
          let tType : fData : otherData = _data
          if tType == "Leaf"
            then return $ Just $ Leaf fData
            else
              if tType == "Node"
                then do
                  left <- loadTree handle
                  right <- loadTree handle
                  let th = case otherData of
                        []        -> "0.0"
                        sData : _ -> sData
                  if isNothing left || isNothing right
                    then return Nothing
                    else
                      return $
                        Just $
                          Node
                            (read fData :: Int)
                            (read th :: Float)
                            (fromJust left)
                            (fromJust right)
                else return Nothing -- error "Tree type not recognized"
        else return Nothing -- error "Invalid input file"

taskOne :: FilePath -> FilePath -> IO ()
taskOne fInput dInput = do
  -- TODO: catch
  tree <-
    withFile
      fInput
      ReadMode
      ( \handle -> do
          loadTree handle
      )
  if isJust tree
    then do
      dataInputContent <- readFile dInput
      forM_ (lines dataInputContent) $ \x -> do
        let stripValues = map (strip (== ' ')) (split x ',')
            values = map (\_x -> (read _x :: Float)) stripValues
        putStrLn $ getClass values $ fromJust tree
    else putStrLn "I was not able to reconstructed the tree from the file."

taskTwo :: FilePath -> IO ()
taskTwo fInput = do
  return ()

main :: IO ()
main = do
  args <- getArgs >>= processArgs
  if taskType args == "-1"
    then do
      taskOne (firstInput args) (dataInput args)
    else do
      taskTwo (firstInput args)
