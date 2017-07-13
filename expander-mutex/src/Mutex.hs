module Mutex where

import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Data.List ((\\), null)
import Data.Set (Set, delete, insert, toList, fromList)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import ListT (ListT)
import qualified ListT as ListT

-- A process ID is simply an int, but the type makes it more expressive.
type Id = Int

-- A system status that contains Ids of processes that are currently idle, waiting or in a critical phase.
data Status = Status { idle :: Set Id, wait :: [Id], critical :: Maybe Id } deriving (Show, Eq, Ord)

-- Converts the status into a tuple. Useful for printing.
toTuple :: Status -> ([Id], [Id], [Id])
toTuple status = (toList $ idle $ status, wait status, maybeToList $ critical $ status) where
	maybeToList (Just x) = [x]
	maybeToList Nothing = []

-- The generic start status of the transition system.
start :: [Id] -> Status
start processes = Status { idle = fromList processes, wait = [], critical = Nothing }

-- A system of statuses with transitions from a status A to a status B if B can be reached directly from A.
type System = Map Status [Status]

-- Writes the system to the command line.
-- There is a line
--      a -> b
-- for each transition from status a to status b.
-- The statuses are printed in tuple form.
writeSystem :: System -> ListT IO ()
writeSystem system = do
    let kvs = Map.assocs system
    (status, ts) <- ListT.fromFoldable $ kvs
    target <- ListT.fromFoldable $ ts
    liftIO $ putStrLn $ "& " ++ show (toTuple status) ++ " -> " ++ show (toTuple target)

-- Calculates all possible transitions for the given status.
transitions :: Status -> [Status]
transitions status = waits ++ enters ++ leaves
    where -- We move each process x from idle to the back of the waiting queue (front of the list).
          statusWithWait id = Status { idle = delete id (idle status), wait = id:(wait status), critical = critical status }
          waits = map statusWithWait (toList $ idle $ status)
          -- If the critical area is empty and there are waiting processes, we add the process
          -- from the front of the waiting queue (back of list).
          enterCritical = Status { idle = idle status, wait = init $ wait $ status, critical = Just $ last $ wait $ status }
          enters = if isNothing (critical status) && not (null $ wait status) then [enterCritical] else []
          -- A process may also leave the critical area if the area isn't empty.
          leaveCritical = Status { idle = insert (fromJust $ critical $ status) (idle status), wait = wait status, critical = Nothing }
          leaves = if isNothing $ critical $ status then [] else [leaveCritical]

-- Filters statuses so that only those are included in the list that have not been visited yet.
unvisited :: System -> [Status] -> [Status]
unvisited system ts = filter (\s -> isNothing $ Map.lookup s system) ts

-- The state that the algorithm starts with.
startState :: Status -> (System, [Status])
startState status = (Map.empty, [status])

-- Takes one state from the unvisited list and adds it to the system.
nextState :: State (System, [Status]) System
nextState = do
    (system, nvs) <- get
    if null nvs
    then return system
    else
        let status = head nvs
            ts = transitions status
            system' = Map.insert status ts system
        in put (system', tail nvs ++ unvisited system' ts) >> nextState

-- Builds a system with the given process IDs.
buildSystem :: [Id] -> System
buildSystem processes = evalState nextState (startState $ start processes)
