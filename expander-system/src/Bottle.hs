{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Bottle where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import TransitionSystem

type BottleState = (Int, Int)
type BottleSystem = Map BottleState [BottleState]

instance SystemNode BottleState BottleState () where
    first _ = (0, 0)
    transitions (l, r) (capacityLeft, capacityRight) = emptyLeft ++ fillLeft ++ emptyRight ++ fillRight ++ transferToLeft ++ transferToRight
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
