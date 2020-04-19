module Lib where

import Prelude

import Data.Int (fromNumber, pow, toNumber)
import Data.Maybe (fromMaybe)
import Math as M
import Range (Area(..), Pos(..), Range(..))

maxPos :: Pos -> Int
maxPos (Pos x y) = max x y

minPos :: Pos -> Int
minPos (Pos x y) = min x y

getMiddlePos :: Area -> Pos
getMiddlePos (Area (Range x1 x2) (Range y1 y2)) = Pos x y 
    where
        x = abs (x1 + x2) / 2
        y = abs (y1 + y2) / 2

abs :: Int -> Int
abs x = fromMaybe 0 $ fromNumber $ M.abs $ toNumber x

sqrt :: Int -> Int
sqrt x = fromMaybe 0 $ fromNumber $ M.sqrt $ toNumber x

dist :: Pos -> Pos -> Int
dist (Pos x1 y1) (Pos x2 y2) = sqrt $ a2 + b2
    where
        a2 = abs (x2 - x1) `pow` 2
        b2 = abs (y2 - y1) `pow` 2

toPos :: forall e. { x :: Int, y :: Int | e } -> Pos
toPos p = Pos p.x p.y