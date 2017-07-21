{-# LANGUAGE TypeFamilies #-}

module Philosophers where

import Data.List

import TransitionSystem

-- Represents the eating state of each philosopher as a bit in an integer.
newtype Table = Table Int
    deriving (Show, Eq, Ord)

-- The table ID of a philosopher.
type PhilosopherId = Int

isEating :: Table -> PhilosopherId -> Bool
isEating (Table table) id = ((table `div` (2 ^ id)) `mod` 2) == 1

setEating :: Table -> PhilosopherId -> Bool -> Table
setEating (Table table) id shouldEat = Table $ updateTable shouldEat (isEating (Table table) id)
    where updateTable True False = table + (2 ^ id)
          updateTable False True = table - (2 ^ id)
          updateTable _ _ = table

instance SystemNode Table where
    type UserData Table = Int
    type Start Table = ()
    toString table count = "(" ++ inner ++ ")"
        where inner = intercalate ", " (map label [0..(count - 1)])
              label id = if isEating table id then "e" else "t"
    first _ = Table 0
    transitions table count = concat (map tryFlip [0..(count - 1)]) where
        tryFlip id =
            if isEating table id
            then [setEating table id False]
            else if not (isEating table (predId id)) && not (isEating table (succId id))
                 then [setEating table id True]
                 else []
        predId id = (id + count - 1) `mod` count
        succId id = (id + 1) `mod` count
