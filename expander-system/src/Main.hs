module Main where

import qualified ListT as ListT
import System.Environment (getArgs)

import TransitionSystem
import Mutex

getProcesses :: [String] -> [Int]
getProcesses (_:processCountString:_) = [0..((read processCountString :: Int) - 1)]
getProcesses _ = [0..5]

main = do
  args <- getArgs
  let mode = head args

  case mode of
    "mutex" -> writeSpec "mutex-base" (first processes :: Mutex.Status) processes ()
            where processes = getProcesses args
    _ -> putStrLn "Please specify a valid mode: mutex, bottle, philosophers"
