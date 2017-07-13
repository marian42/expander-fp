module Main where

import qualified ListT as ListT
import System.Environment (getArgs)

import Bottle

getCapacity :: [String] -> Status
getCapacity (lString:rString:_) = ((read lString :: Int), (read rString :: Int))
getCapacity _ = (3, 5)

main = do
  args <- getArgs
  let capacity = getCapacity args

  putStrLn "specs: bottle"
  putStrLn "axioms:"

  putStrLn $ "start == " ++ toString capacity

  let system = buildSystem capacity
  
  ListT.toList $ writeSystem system

  putStrLn $ "States: " ++ show (length system)
