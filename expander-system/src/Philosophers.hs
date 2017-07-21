{-# LANGUAGE TypeFamilies #-}

module Philosophers where

import Data.List

import TransitionSystem

-- Represents the eating state of each philosopher as a bit in an integer.
type Table = Int

-- The table ID of a philosopher.
type PhilosopherId = Int

isEating :: Table -> PhilosopherId -> Bool
isEating table id = ((table `div` (2 ^ id)) `mod` 2) == 1

setEating :: Table -> PhilosopherId -> Bool -> Table
setEating table id shouldEat = updateTable shouldEat (isEating table id)
    where updateTable True False = table + (2 ^ id)
          updateTable False True = table - (2 ^ id)
          updateTable _ _ = table

instance SystemNode Int where
    type UserData Int = Int
    type Start Int = ()
    toString table count = "(" ++ inner ++ ")"
        where inner = intercalate ", " (map label [0..(count - 1)])
              label id = if isEating table id then "e" else "t"
    first _ = 0
    transitions table count = concat (map tryFlip [0..(count - 1)]) where
        tryFlip id =
            if isEating table id
            then [setEating table id False]
            else if not (isEating table (predId id)) && not (isEating table (succId id))
                 then [setEating table id True]
                 else []
        predId id = (id + count - 1) `mod` count
        succId id = (id + 1) `mod` count
