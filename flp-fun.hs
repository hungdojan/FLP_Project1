-- This module contains utilities used for processing program arguments
-- and the main function.
--
-- This source code serves as the submission to the first assignment
-- of class FLP at FIT, BUT 2023/24
--
-- @author  Hung Do
-- @date    04/03/2024
-- @file    main.hs

module Main (main) where

import           System.Environment (getArgs)
import           TaskOne
import           TaskTwo

data Args = Args
  { taskType   :: String,
    firstInput :: FilePath,
    dataInput  :: FilePath
  }
  deriving (Show)

-- | Load arguments
processArgs :: [String] -> Args
processArgs (tT@"-1" : fI : dI : _) = Args tT fI dI
processArgs (tT@"-2" : fI : _)      = Args tT fI ""
processArgs _                       = error "Wrong input"

main :: IO ()
main = do
  tmp <- getArgs
  let args = processArgs tmp

  if taskType args == "-1"
    then do
      taskOne (firstInput args) (dataInput args)
    else do
      taskTwo (firstInput args)
