module Main where

import Codingame
import Simulation.Board
import Simulation.Data
import Data.Vector as V

main :: IO ()
-- main = test
main = bundle

test :: IO ()
test = print $ Prelude.reverse $ loop1 (0,0) 10 []

loop1 :: Pos -> Int -> [Pos] -> [Pos]
loop1 pos depth acc
    | depth == 0 = acc
    | otherwise = 
        let sim  = snd $ sim1 pos
            acc' = sim : acc
        in loop1 sim (depth - 1) acc'

sim1 :: Pos -> (Int, Pos)
sim1 pos = simulate board1 (0, 100, pos, singleton (0,4))

board1 :: Board
board1 = fromList $ fmap fromList
           [[Air, Air, Air, Air, Air],
            [Air, Air, Air, Air, Air],
            [Air, Air, Air, Air, Air],
            [Air, Air, Air, Air, Air],
            [Mine,Air, Air, Mine,Air]]

emptyBoard :: Board
emptyBoard = V.generate 5 (\_ -> V.replicate 5 Air) 
