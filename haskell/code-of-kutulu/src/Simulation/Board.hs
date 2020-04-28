module Simulation.Board
    ( simulate
    , evalGameState
    ) where

-- import Prelude
import qualified Data.Vector as V
import Control.Monad.State as S
import Control.Monad.State.Class
import Data.List as L
import Simulation.Data
import Simulation.Lib

searchDepth = 6

-- TODO: Check if tailrec
simulate :: Board -> GameState -> (Int, GameState)
simulate = simulateMove searchDepth

simulateMove :: Int -> Board -> GameState -> (Int, GameState)
simulateMove depth board state@(hero@(Explorer ownId pos sanity plans), enemies)
    | depth == 0 =
        let state' = evalMove board state
        in (evalGameState state', state')
    | otherwise =
        let state' = evalMove board state
            -- bPos = boardPos board pos
            moves = V.filter (posValid board state) $ possibleMoves pos
            vals = fmap (\pos' -> simulateMove (depth - 1) board (updatePos pos' state')) moves
            valsWithOldPos = if depth == searchDepth
                then vals   -- return position of submove on first level
                else V.zip (fmap fst vals) $ fmap (updatePos pos . snd) vals  -- return starting position otherwise  -- pos'
        in L.maximumBy (\(v1, _) (v2, _) -> compare v1 v2) valsWithOldPos

updatePos :: Pos -> GameState -> GameState
updatePos pos ((Explorer id _ sanity plans), enemies) = ((Explorer id pos sanity plans), enemies)

-- update State according to hero position on board
-- executed every move
evalMove :: Board -> GameState -> GameState
evalMove board state@(hero@(Explorer id pos sanity plans), enemies) = evalDeath $ evalEnemies $ evalEffects evalSanity
    where
        evalSanity :: GameState
        evalSanity
            | any (< 3) $ fmap (dist pos) (fmap wandererPos enemies) = (Explorer id pos (sanity - 1) plans, enemies)
            | otherwise = (Explorer id pos (sanity - 3) plans, enemies)
        evalEffects :: GameState -> GameState
        evalEffects state'@(hero'@(Explorer id' pos' sanity' plans'), enemies')
            | entity == Empty = (hero', enemies')
            | entity == SpawnWanderer = (hero', enemies')
            | entity == Wall = state -- should never happen
            where
                entity = boardPos board pos'
        -- TODO: Gegner verliert auch Leben
        evalEnemies :: GameState -> GameState
        evalEnemies state'@((Explorer id' pos' sanity' plans'), enemies')
            | any (< 2) distFromWanderer = (Explorer id' pos' (sanity' - 20) plans', enemies')
            | any (< 3) distFromWanderer = (Explorer id' pos' (sanity' - 10) plans', enemies')
            | any (< 4) distFromWanderer = (Explorer id' pos' (sanity' - 5) plans', enemies')
            | otherwise = state'
            where
                distFromWanderer = fmap (dist $ pos') (fmap wandererPos enemies')
        evalDeath :: GameState -> GameState
        evalDeath state'@((Explorer id' pos' sanity' plans'), enemies')
            | sanity' < 1 = ((Explorer id' pos' (-999) plans'), enemies')   -- TODO: starting position is not 0,0 but spawnpoint, enemy gets own mines
            | otherwise = state'

-- retuns the evalutaion of the current move
-- executed if maximum depth is reached
evalGameState :: GameState -> Int
evalGameState ((Explorer _ pos sanity plans), enemies) =
    sanity
    -- enemyDist
    -- where
    --     minMineDist = minimum $ fmap (dist hero) eMines

-- get BoardInternalEntity Enum of Pos on BoardInternal
boardPos :: Board -> Pos -> BoardEntity
boardPos board (x,y) = (board V.! y) V.! x

posValid :: Board -> GameState -> Pos -> Bool
posValid board (hero, enemies) pos@(x,y) = onBoardInternal && boardPos' /= Wall
    where
        width = length $ V.head board
        height = length board
        boardPos' = boardPos board pos
        onBoardInternal = x >= 0 && x < width && y >= 0 && y < height

possibleMoves :: Pos -> V.Vector Pos
possibleMoves (x,y) = V.fromList [ (x+1, y), (x, y+1), (x-1, y), (x, y-1) ]


data Tree v = Node v (Tree v) | Leaf v