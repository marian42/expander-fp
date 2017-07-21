{-# LANGUAGE TypeFamilies #-}

module TransitionSystem where

import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Data.List ((\\), null)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import ListT (ListT)
import qualified ListT as ListT

-- A system of nodes a that are connected via transitions, one transition list for each key in the map.
type TransitionSystem a = Map a [a]

-- a is the type of the node value of the system.
class (Eq a, Ord a) => SystemNode a where
    -- UserData a is a user-data type that is carried as a constant throughout the computation. 
    type UserData a :: *
    
    -- Start a is the type of the starting configuration that computes the first node.
    type Start a :: *

    -- Creates a String version of the node value, used for writing the system to the standard output.
    toString :: a -> UserData a -> String

    -- The first node of the transition system.
    first :: Start a -> a

    -- Computes a list of nodes reached from a, given the user-data b.
    transitions :: a -> UserData a -> [a]

-- Filters nodes so that only those are included in the list that have not been visited yet.
unvisited :: SystemNode a => TransitionSystem a -> [a] -> [a]
unvisited system ts = filter (\node -> isNothing $ Map.lookup node system) ts

-- The state that the algorithm starts with.
startState :: SystemNode a => a -> (TransitionSystem a, [a])
startState node = (Map.empty, [node])

-- Takes one state from the unvisited list and adds it to the system.
nextState :: SystemNode a => UserData a -> State (TransitionSystem a, [a]) (TransitionSystem a)
nextState cdata = do
    (system, nvs) <- get
    if null nvs
    then return system
    else
        let node = head nvs
            ts = transitions node cdata
            system' = Map.insert node ts system
        in put (system', tail nvs ++ unvisited system' ts) >> nextState cdata

-- Builds a system with the given start value and constant user-data.
-- For the argument 'start', see below.
buildSystem :: SystemNode a => a -> Start a -> UserData a -> TransitionSystem a
buildSystem start sdata cdata = evalState (nextState cdata) (startState start)

-- Writes the system to the command line.
-- There is a line
--      a -> b
-- for each transition from node a to node b.
writeSystem :: SystemNode a => TransitionSystem a -> UserData a -> ListT IO ()
writeSystem system cdata = do
    let kvs = Map.assocs system
    (node, ts) <- ListT.fromFoldable $ kvs
    target <- ListT.fromFoldable $ ts
    liftIO $ putStrLn $ "& " ++ toString node cdata ++ " -> " ++ toString target cdata

-- The argument 'start' is sadly necessary until a better solution has been found
-- so that we can specify the type variable a.
writeSpec :: SystemNode a => String -> String -> a -> UserData a -> Start a -> IO ()
writeSpec specName extra start cdata sdata = do
    -- Write the header text.
    putStrLn ("specs: " ++ specName)
    putStr extra
    putStrLn "axioms:"

    -- We need to define the start state so that Expander can properly work this out.
    -- Also adds some kind of first axiom, so that we don't need to treat
    -- the superfluous & added in writeSystem in a special way.
    putStrLn $ "start == " ++ toString start cdata

    let system = buildSystem start sdata cdata

    -- Write the transitions of the system.
    ListT.toList (writeSystem system cdata)

    return ()
