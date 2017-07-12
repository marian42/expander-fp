module Philosophers where

import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Data.List ((\\), null)
import Data.Set (Set, delete, insert, toList, fromList)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import ListT (ListT)
import qualified ListT as ListT

type Status = Int

isEating :: Status -> Int -> Bool
isEating satus philosopher = ((satus `div` 2 ^ philosopher) `mod` 2) == 1

setEating :: Status -> Int -> Bool -> Status
setEating satus philosopher eating = satus + (2 ^ philosopher) * ((if eating then 1 else 0) - if isEating satus philosopher then 1 else 0)

transitions :: Status -> Int -> [Status]
transitions current count = concat (map tryFlip  [0..(count - 1)]) where
	tryFlip philosopher = if isEating current philosopher
		then [setEating current philosopher False]
		else if not (isEating current ((philosopher + count - 1) `mod` count))
			&& not (isEating current ((philosopher + 1) `mod` count))
			then [setEating current philosopher True]
			else []

type System = Map Status [Status]

unvisited :: System -> [Status] -> [Status]
unvisited system ts = filter (\s -> isNothing $ Map.lookup s system) ts

startState :: (System, [Status])
startState = (Map.empty, [0])

nextState :: Int -> State (System, [Status]) System
nextState count = do
    (system, nvs) <- get
    if null nvs
    then return system
    else
        let status = head nvs
            ts = transitions status count
            system' = Map.insert status ts system
        in put (system', tail nvs ++ unvisited system' ts) >> nextState count

buildSystem :: Int -> System
buildSystem count = evalState (nextState count) startState

toString :: Status -> Int -> String
toString status count | count > 1 = toString status (count - 1) ++ " " ++ if isEating status (count - 1) then "E" else "T"
toString satus 1 = if isEating satus 0 then "E" else "T"
toString status count | count < 1 = "err"

writeSystem :: System -> Int -> ListT IO ()
writeSystem system count = do
    let kvs = Map.assocs system
    (status, ts) <- ListT.fromFoldable $ kvs
    target <- ListT.fromFoldable $ ts
    liftIO $ putStrLn $ "& " ++ toString status count ++ " -> " ++ toString target count