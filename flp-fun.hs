-- This module contains utilities used for processing program arguments
-- and the main function.
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
processArgs :: [String] -> IO Args
processArgs (tT@"-1" : fI : dI : _) = return (Args tT fI dI)
processArgs (tT@"-2" : fI : _)      = return (Args tT fI "")
processArgs _                       = error "Wrong input"

main :: IO ()
main = do
  args <- getArgs >>= processArgs
  if taskType args == "-1"
    then do
      taskOne (firstInput args) (dataInput args)
    else do
      taskTwo (firstInput args)
