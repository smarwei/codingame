module Main where

import Prelude

import Data.Array (any, drop, head, length, sortBy)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Lib (dist, toPos)
import Range (Pos(..))
import Reader (Player, Human, parseInput)

main :: Effect Unit
main = do
    loop Nothing

loop :: Maybe Pos -> Effect Unit
loop target = do
    input <- parseInput
    loop' input.player input.humans target

loop' :: Player -> Array Human -> Maybe Pos -> Effect Unit
loop' player humans target = do
    let nearestHuman = sortBy (\h1 h2 -> compare (dist (toPos h1) playerPos) (dist (toPos h2) playerPos)) humans
    let sndHuman = if length nearestHuman > 1 then drop 1 nearestHuman else nearestHuman

    let target' = case target of
            Just t -> if targetAlive then [t] else map toPos sndHuman
            Nothing -> if length nearestHuman == 2 then map toPos sndHuman else map toPos nearestHuman

    let pos = case head target' of
            Just p  -> p
            Nothing -> Pos 6000 6000

    log $ show pos

    loop $ Just pos

    where
        playerPos :: Pos
        playerPos = toPos player

        targetAlive :: Boolean
        targetAlive = case target of
            Just (Pos tx ty) -> any (\h -> h.x == tx && h.y == ty) humans 
            Nothing -> false
            