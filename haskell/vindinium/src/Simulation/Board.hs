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

searchDepth = 8

-- fromPlayerBoard :: Board -> BoardInternal
-- fromPlayerBoard pBoardInternal = fmap (fmap $ fromEnum) asVector
--     where asVector = V.fromList $ fmap V.fromList pBoardInternal

-- All valid board positions are possible. For example the player could move
-- back and forth between two fields infinitely
-- Caution: if the player moved inside a Tavern or Mine he needs to be reset to his initial position afterwards
-- TODO: Check if tailrec
simulate :: Board -> GameState -> (Int, GameState)
simulate board = simulateMove board (-1,-1) searchDepth

simulateMove :: Board -> Pos -> Int -> GameState -> (Int, GameState)
simulateMove board prevPos depth state@(_,_,pos,_,_,_)
    | depth == 0 =
        let state' = evalMove board state
        in (evalGameState state', state')
    | otherwise =
        let state' = evalMove board state
            bPos = boardPos board pos
            -- der Trick: in einem Zug muss die Minenposition zurueckgegeben werden, die Position des Helden
            -- aendert sich aber nicht. Im naechsten Zug will der Held dann nicht mehr die Mine erobern.
            pos' = if bPos == Tavern || bPos == Mine then prevPos else pos      -- move back out of tavern/mine
            moves = V.filter (posValid board state) $ possibleMoves pos'
            vals = fmap (\pos'' -> simulateMove board pos' (depth-1) (updatePos pos'' state')) moves
            valsWithOldPos = if depth == searchDepth
                then vals   -- return position of submove on first level
                else V.zip (fmap fst vals) $ fmap (updatePos pos . snd) vals  -- return starting position otherwise  -- pos'
        in L.maximumBy (\(v1, _) (v2, _) -> compare v1 v2) valsWithOldPos

updatePos :: Pos -> GameState -> GameState
updatePos pos (gold, life, _, oMines, eMines, enemies) = (gold, life, pos, oMines, eMines, enemies)

-- update State according to hero position on board
-- executed every move
evalMove :: Board -> GameState -> GameState
evalMove board state@(gold, life, pos, oMines, eMines, enemies) = evalDeath $ evalEnemies evalBuildings
    where
        evalBuildings
            | entity == Air = (gold + length oMines, thirst life, pos, oMines, eMines, enemies)
            | entity == SpawnPoint = (gold + length oMines, thirst life, pos, oMines, eMines, enemies)
            | entity == Tavern =
                if gold >= 2 then
                   ( gold + length oMines - 2
                   , min 100 (life + 50)    -- TODO: Check if life is +19
                   , pos
                   , oMines
                   , eMines
                   , enemies
                   )
                else 
                   ( gold + length oMines
                   , thirst life
                   , pos
                   , oMines
                   , eMines
                   , enemies
                   )
            | entity == Mine =
                let addMine = pos `V.notElem` oMines
                    oMines' = if addMine then V.cons pos oMines else oMines
                in
                    ( gold + length oMines'
                    , if addMine then thirst life - 20 else thirst life
                    , pos
                    , oMines'
                    , eMines
                    , enemies
                    )
            | entity == Wall = state -- should never happen
            where
                entity = boardPos board pos
        -- TODO: Gegner verliert auch Leben
        evalEnemies :: GameState -> GameState
        evalEnemies state'@(gold', life', pos', oMines', eMines', enemies')
            | any (<3) $ fmap (dist pos') enemies' = (gold', life' - 20, pos', oMines', eMines', enemies')
            | otherwise = state'
        evalDeath state'@(gold', life', pos', oMines', eMines', enemies')
            | life' < 5 = (gold', 100, (0,0), V.empty, eMines', enemies')   -- TODO: starting position is not 0,0 but spawnpoint, enemy gets own mines
            | otherwise = state'

thirst life = max 1 (life - 1)

-- retuns the evalutaion of the current move
-- executed if maximum depth is reached
evalGameState :: GameState -> Int
evalGameState (gold, life, hero, oMines, eMines, _) = -- TODO: Warum macht nur mines quatsch?
    gold
    + (life `div` 10)
    + length oMines
    - minMineDist
    where
        minMineDist = minimum $ fmap (dist hero) eMines

-- get BoardInternalEntity Enum of Pos on BoardInternal
boardPos :: Board -> Pos -> BoardEntity
boardPos board (x,y) = (board V.! y) V.! x

posValid :: Board -> GameState -> Pos -> Bool
posValid board (_,_,_,mines,_,_) pos@(x,y) = onBoardInternal && boardPos' /= Wall && pos `notElem` mines
    where
        size = length board
        boardPos' = boardPos board pos
        onBoardInternal = x >= 0 && x < size && y >= 0 && y < size

possibleMoves :: Pos -> V.Vector Pos
possibleMoves (x,y) = V.fromList [ (x+1, y), (x, y+1), (x-1, y), (x, y-1) ]


data Tree v = Node v (Tree v) | Leaf v