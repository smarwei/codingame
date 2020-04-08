module Main where

import Prelude

import Data.Array (filter, head, length, reverse, sort, sortBy, (!!), (..))
import Data.Int (fromNumber, toNumber)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Console (error, log)
import Math as M
import Range (Range(..), Area(..), Pos(..), range, fromPos)
import Reader (GameInput, parseInput, readline)

main :: Effect Unit
main = do
    input <- parseInput
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

    let area' = shrinkArea dir area (fromPos pos)
    let (Pos x y) = getMiddlePos area'
    let input' = input { x = x, y = y }

    log $ show x <> " " <> show y

    loop input' area'

    where
        shrinkArea :: String -> Area -> Range -> Area
        shrinkArea "U"  a (Range x y) = Area (Range x x) (yUp a y)
        shrinkArea "D"  a (Range x y) = Area (Range x x) (yDown a y)
        shrinkArea "R"  a (Range x y) = Area (xRight a x) (Range y y)
        shrinkArea "L"  a (Range x y) = Area (xLeft a x) (Range y y)
        shrinkArea "UR" a (Range x y) = Area (xRight a x) (yUp a y)
        shrinkArea "DR" a (Range x y) = Area (xRight a x) (yDown a y)
        shrinkArea "DL" a (Range x y) = Area (xLeft a x) (yDown a y)
        shrinkArea "UL" a (Range x y) = Area (xLeft a x) (yUp a y)
        shrinkArea  _   _     _     = Area (Range 9999 9999) (Range 9999 9999)
        xRight (Area (Range x1 x2) _) x = Range (x+1) (input.width-1)
        xLeft  (Area (Range x1 x2) _) x = Range (x-1) 0
        yUp (Area _ (Range y1 y2)) y = Range (y-1) 0
        yDown (Area _ (Range y1 y2)) y = Range (y+1) (input.height-1)

maxPos :: Pos -> Int
maxPos (Pos x y) = max x y

minPos :: Pos -> Int
minPos (Pos x y) = min x y

getMiddlePos :: Area -> Pos
getMiddlePos (Area (Range x1 x2) (Range y1 y2)) = Pos x y 
    where
        x = abs (x1 + x2) / 2
        y = abs (y1 + y2) / 2
--        minX = min \(Pos x y) -> x
--        minY = min \(Pos x y) -> y
--        maxX = max \(Pos x y) -> x
--        maxY = max \(Pos x y) -> y

abs :: Int -> Int
abs x = fromMaybe (-1) $ fromNumber $ M.abs $ toNumber x

-- nextMove :: Building -> Pos -> Array Pos -> Pos
-- nextMove building pos bombDirs =
--     fromMaybe (Pos 100 100) $ head $ sortBy comparePos validPos
--   where comparePos :: Pos -> Pos -> Ordering
--         comparePos p1 p2 = compare (posVal p2) (posVal p1)
--         validPos :: Array Pos
--         validPos = filter (posValid building) possiblePos
--         possiblePos = map (addPos pos) bombDirs
-- 
-- posValid :: Building -> Pos -> Boolean
-- posValid (Building w h) (Pos x y) = x <= w && y <= h
-- 
-- posVal :: Pos -> Int
-- posVal (Pos x y) = fromMaybe 0 $ fromNumber $ absNum x + absNum y
--     where absNum = abs <<< toNumber
-- 
-- addPos :: Pos -> Pos -> Pos
-- addPos (Pos x1 y1) (Pos x2 y2) = Pos (x1+x2) (y1+y2)
