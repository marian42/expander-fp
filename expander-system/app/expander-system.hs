module Main where

import qualified ListT as ListT
import System.Environment (getArgs)

import TransitionSystem
import Mutex
import Bottle
import Philosophers

main = do
  args <- getArgs
  let mode = head args

  case mode of
    "mutex" -> writeSpec "mutex-base" "" (first processes :: Mutex.Status) () processes
            where processes = getProcesses args
                  getProcesses (_:processCountString:_) = [0..((read processCountString :: Int) - 1)]
                  getProcesses _ = [0..5]
    "bottle" -> writeSpec "bottle" "" (first () :: Bottle.BottleState) capacity ()
            where capacity = BottleState $ getCapacity args
                  getCapacity (_:lString:rString:_) = ((read lString :: Int), (read rString :: Int))
                  getCapacity _ = (3, 5)
    "philosophers" -> writeSpec "modal" "constructs: t e\n" (first () :: Philosophers.Table) count ()
            where count = getCount args
                  getCount (_:countString:_) = (read countString :: Int)
                  getCount _ = 5
    _ -> putStrLn "Please specify a valid mode: mutex, bottle, philosophers"
