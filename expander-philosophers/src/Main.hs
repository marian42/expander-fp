module Main where

import qualified ListT as ListT
import System.Environment (getArgs)

import Philosophers

getProcesses :: [String] -> Int
getProcesses (processCountString:_) = (read processCountString :: Int)
getProcesses _ = 5

main = do
  args <- getArgs
  let count = getProcesses args

  putStrLn "specs: modal"
  putStrLn "constructs: t e"
  putStrLn "axioms:"

  putStrLn $ "start == " ++ toString 0 count

  let system = buildSystem count
  
  ListT.toList $ writeSystem system count

  putStrLn $ "States: " ++ show (length system)
