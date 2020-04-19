module Lib where

import Prelude

import Data.Int (fromNumber, pow, toNumber)
import Data.Maybe (Maybe(..), fromJust)
import Math as M
import Partial.Unsafe (unsafePartial)
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
abs x = unsafePartial $ fromJust $ fromNumber $ M.abs $ toNumber x

sqrt :: Int -> Int
sqrt x = unsafePartial $ fromJust $ fromNumber $ M.floor $ M.sqrt $ toNumber x

dist :: forall a b. { x :: Int, y :: Int | a } -> { x :: Int, y :: Int | b } -> Int
dist p1 p2 = sqrt $ a2 + b2
    where
        a2 = abs (p2.x - p1.x) `pow` 2
        b2 = abs (p2.y - p1.y) `pow` 2

toPos :: forall e. { x :: Int, y :: Int | e } -> Pos
toPos p = Pos p.x p.y

-- addNode :: forall k. Ord k => G.Graph k k -> k -> G.Graph k k
-- addNode g v = G.insertVertex v v g
-- infixl 5 addNode as <+>
-- 
-- addEdge :: forall k v. Ord k => Maybe (G.Graph k v) -> Array k -> Maybe (G.Graph k v)
-- addEdge (Just g) [a,b] = case G.insertEdge a b g of
--     Just g' -> Just g'
--     Nothing -> Just g
-- addEdge _ _ = Nothing