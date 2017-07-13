module Bottle where

import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Data.List ((\\), null)
import Data.Set (Set, delete, insert, toList, fromList)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import ListT (ListT)
import qualified ListT as ListT

type Status = (Int, Int)

transitions :: Status -> Status -> [Status]
transitions (l, r) (capacityLeft, capacityRight) = emptyLeft ++ fillLeft ++ emptyRight ++ fillRight ++ transferToLeft ++ transferToRight where
	emptyLeft
		| l > 0 = [(0, r)]
		| otherwise = [] 
	emptyRight
		| r > 0 = [(l, 0)]
		| otherwise = []
	fillLeft
		| l < capacityLeft = [(capacityLeft, r)]
		| otherwise = []
	fillRight
		| r < capacityRight = [(l, capacityRight)]
		| otherwise = []
	transferToLeft
		| r > 0 && l < capacityLeft = [(min (l + r) capacityLeft, l + r - min (l + r) capacityLeft)]
		| otherwise = []
	transferToRight
		| l > 0 && r < capacityRight = [(l + r - min(l + r) capacityRight, min(l + r) capacityRight)]
		| otherwise = []

type System = Map Status [Status]

unvisited :: System -> [Status] -> [Status]
unvisited system ts = filter (\s -> isNothing $ Map.lookup s system) ts

startState :: (System, [Status])
startState = (Map.empty, [(0, 0)])

nextState :: Status -> State (System, [Status]) System
nextState capacity = do
    (system, nvs) <- get
    if null nvs
    then return system
    else
        let status = head nvs
            ts = transitions status capacity
            system' = Map.insert status ts system
        in put (system', tail nvs ++ unvisited system' ts) >> nextState capacity

buildSystem :: Status -> System
buildSystem capacity = evalState (nextState capacity) startState

toString :: Status -> String
toString (l, r) = "(" ++ show l ++ ", " ++ show r ++ ")"

writeSystem :: System -> ListT IO ()
writeSystem system = do
    let kvs = Map.assocs system
    (status, ts) <- ListT.fromFoldable $ kvs
    target <- ListT.fromFoldable $ ts
    liftIO $ putStrLn $ "& " ++ toString status ++ " -> " ++ toString target