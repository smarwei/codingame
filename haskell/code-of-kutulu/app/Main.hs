module Main where

import System.Environment
import Codingame
import Simulation.Board
import Simulation.Data
import qualified Data.Sequence as S

main :: IO ()
main = do
    bundle
    -- print $ sim1 (2,4)
    --test

test :: IO ()
test = do
    args <- getArgs
    let simDepth = if Prelude.length args > 0 then read $ Prelude.head args :: Int else 3
    print $ Prelude.reverse $ loop1 (0,0) simDepth []

loop1 :: Pos -> Int -> [Pos] -> [Pos]
loop1 pos depth acc
    | depth == 0 = acc
    | otherwise = 
        let sim  = snd $ sim1 pos
            acc' = sim : acc
        in loop1 sim (depth - 1) acc'

sim1 :: Pos -> (Float, Pos)
sim1 pos = (\(val, (Explorer _ pos _ _, _)) -> (val, pos)) (simulate board1 (Explorer 0 (0,0) 100 2, S.empty))

board1 :: Board
board1 = S.fromList $ fmap S.fromList
           [[Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty]]
