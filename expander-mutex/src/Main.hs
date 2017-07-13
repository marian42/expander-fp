module Main where

import qualified ListT as ListT
import System.Environment (getArgs)

import Mutex

-- 0..5: 3913 statuses
-- 0..6: 27399 statuses

getProcesses :: [String] -> [Int]
getProcesses (processCountString:_) = [0..((read processCountString :: Int) - 1)]
getProcesses _ = [0..5]

main = do
  args <- getArgs
  
  let processes = getProcesses args

  -- Write the header text.
  putStrLn "specs: mutex-base"
  putStrLn "axioms:"

  -- We need to define the start state so that Expander can properly work this out.
  -- Also adds some kind of first axiom, so that we don't need to treat
  -- the superfluous & added in writeSystem in a special way.
  putStrLn $ "start == (" ++ show processes ++ ",[],[])"

  let system = buildSystem processes

  -- Write the transitions of the system.
  if ("-o" `elem` args)
	then ListT.toList $ writeSystem $ system
	else return []

  putStrLn $ "States: " ++ show (length system)
