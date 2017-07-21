{-# LANGUAGE TypeFamilies #-}
module Bottle where

import TransitionSystem

newtype BottleState = BottleState (Int, Int)
    deriving (Show, Eq, Ord)

instance SystemNode BottleState where
    type UserData BottleState = BottleState
    type Start BottleState = ()
    toString state _ = show state
    first _ = BottleState (0, 0)
    transitions (BottleState (l, r)) (BottleState (capacityLeft, capacityRight)) = map BottleState $ emptyLeft ++ fillLeft ++ emptyRight ++ fillRight ++ transferToLeft ++ transferToRight
      where
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
