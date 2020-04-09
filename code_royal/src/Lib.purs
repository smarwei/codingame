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

dist :: forall a b. { x :: Int, y :: Int | a } -> { x :: Int, y :: Int | b } -> Int
dist p1 p2 = sqrt $ a2 + b2
    where
        a2 = abs (p2.x - p1.x) `pow` 2
        b2 = abs (p2.y - p1.y) `pow` 2

toPos :: forall e. { x :: Int, y :: Int | e } -> Pos
toPos p = Pos p.x p.y