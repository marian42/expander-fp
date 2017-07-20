{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Mutex where

import Data.Set (Set, delete, insert, toList, fromList)
import Data.Maybe (isNothing, isJust, fromJust)

import TransitionSystem

-- A process ID is simply an int, but the type makes it more expressive.
type Id = Int

-- A system status that contains Ids of processes that are currently idle, waiting or in a critical phase.
data Status = Status { idle :: Set Id, wait :: [Id], critical :: Maybe Id } deriving (Eq, Ord)

-- Converts the status into a tuple. Useful for printing.
toTuple :: Status -> ([Id], [Id], [Id])
toTuple status = (toList $ idle $ status, wait status, maybeToList $ critical $ status)
    where maybeToList (Just x) = [x]
          maybeToList Nothing = []

instance SystemNode Status () [Id] where
    toString status _ = show . toTuple $ status
    first processes = Status { idle = fromList processes, wait = [], critical = Nothing }
    transitions status _ = waits ++ enters ++ leaves
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
