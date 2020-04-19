module Main where

import Prelude

import Data.Array (filter, head, length, reverse, sort, sortBy, (!!), (..))
import Data.Int (fromNumber, toNumber)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Console (error, log)
import Math as M
import Range (Range(..), Area(..), Pos(..), range)
import Reader (GameInput, parseInput, readline)

main :: Effect Unit
main = do
    input <- parseInput
    error $ show input
    loop input (calcArea input)

calcArea :: GameInput -> Area
calcArea input = Area rX rY
    where
        rX = range 0 $ input.width - 1
        rY = range 0 $ input.height - 1

loop :: GameInput -> Area -> Effect Unit
loop input area = do
    dir <- readline

    let pos = Pos input.x input.y

    let area' = shrinkArea dir area pos
    error $ show area
    error $ show area'
    let (Pos x y) = getMiddlePos area'
    let input' = input { x = x, y = y }

    log $ show x <> " " <> show y

    loop input' area'

    where
        shrinkArea :: String -> Area -> Pos -> Area
        shrinkArea "U"  a (Pos x y) = Area (range x x) (yUp a y)
        shrinkArea "D"  a (Pos x y) = Area (range x x) (yDown a y)
        shrinkArea "R"  a (Pos x y) = Area (xRight a x) (range y y)
        shrinkArea "L"  a (Pos x y) = Area (xLeft a x) (range y y)
        shrinkArea "UR" a (Pos x y) = Area (xRight a x) (yUp a y)
        shrinkArea "DR" a (Pos x y) = Area (xRight a x) (yDown a y)
        shrinkArea "DL" a (Pos x y) = Area (xLeft a x) (yDown a y)
        shrinkArea "UL" a (Pos x y) = Area (xLeft a x) (yUp a y)
        shrinkArea  _   _     _     = Area (range 9999 9999) (range 9999 9999)
        xRight (Area (Range xL xR) _) x = range (x+1) xR
        xLeft  (Area (Range xL xR) _) x = range xL (x-1)
        yUp (Area _ (Range yU yL)) y = range yU (y-1)
        yDown (Area _ (Range yU yL)) y = range (y+1) yL

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
abs x = fromMaybe (-1) $ fromNumber $ M.abs $ toNumber x
