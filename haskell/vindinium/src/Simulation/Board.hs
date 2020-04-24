module Simulation.Board
    ( simulate
    ) where

-- import Prelude
import qualified Data.Vector as V
import Control.Monad.State as S
import Control.Monad.State.Class
import Data.List as L
import Simulation.Data

searchDepth = 8

-- fromPlayerBoard :: Board -> BoardInternal
-- fromPlayerBoard pBoardInternal = fmap (fmap $ fromEnum) asVector
--     where asVector = V.fromList $ fmap V.fromList pBoardInternal

-- All valid board positions are possible. For example the player could move
-- back and forth between two fields infinitely
-- Caution: if the player moved inside a Tavern or Mine he needs to be reset to his initial position afterwards
-- TODO: Check if tailrec
simulate :: Board -> GameState -> (Int, Pos)
simulate board = simulateMove board (-1,-1) searchDepth

simulateMove :: Board -> Pos -> Int -> GameState -> (Int, Pos)
simulateMove board prevPos depth state@(_,_,pos,_)
    | depth == 0 =
        let state' = evalMove board state
        in (evalGameState state', pos)
    | otherwise =
        let state' = evalMove board state
            bPos = boardPos board pos
            -- der Trick: in einem Zug muss die Minenposition zurueckgegeben werden, die Position des Helden
            -- aendert sich aber nicht. Im naechsten Zug will der Held dann nicht mehr die Mine erobern.
            --pos' = if bPos == Tavern || bPos == Mine then prevPos else pos      -- move back out of tavern/mine
            vals = fmap (\pos' -> simulateMove board pos (depth-1) (updatePos pos' state')) moves   -- before pos' 
            valsWithOldPos = if depth == searchDepth
                then vals   -- return position of submove on first level
                else zip (fmap fst vals) (replicate 4 pos)  -- return starting position otherwise  -- before pos'
        in L.maximumBy (\(v1, _) (v2, _) -> compare v1 v2) valsWithOldPos
    where
        moves :: [Pos]
        moves = filter (posValid board state) $ possibleMoves pos

updatePos :: Pos -> GameState -> GameState
updatePos pos (gold, life, _, mines) = (gold, life, pos, mines)

-- update State according to hero position on board
-- executed every move
evalMove :: Board -> GameState -> GameState
evalMove board state@(gold, life, pos, mines) = evalBuildings
    where
        evalBuildings
            | entity == Air = (gold + length mines, life-1, pos, mines)
            | entity == SpawnPoint = (gold + length mines, life-1, pos, mines)
            | entity == Tavern =
                if gold >= 2 then
                   ( gold + length mines - 2
                   , min 100 (life + 50)    -- TODO: Check if life is +19
                   , pos
                   , mines
                   )
                else 
                   ( gold + length mines
                   , life - 1
                   , pos
                   , mines
                   )
            | entity == Mine =
                let addMine = pos `V.notElem` mines
                    mines' = if addMine then V.cons pos mines else mines
                in
                    ( gold + length mines'
                    , life - 1
                    , pos
                    , mines'
                    )
            | entity == Wall = state -- should never happen
            where
                entity = boardPos board pos

-- retuns the evalutaion of the current move
-- executed if maximum depth is reached
evalGameState :: GameState -> Int
evalGameState (gold, _, _, mines) = gold + length mines * 10

-- get BoardInternalEntity Enum of Pos on BoardInternal
boardPos :: Board -> Pos -> BoardEntity
boardPos board (x,y) = (board V.! y) V.! x

posValid :: Board -> GameState -> Pos -> Bool
posValid board (_, _, _, mines) pos@(x,y) = onBoardInternal && boardPos' /= Wall && boardPos' /= Tavern && pos `notElem` mines
    where
        size = length board
        boardPos' = boardPos board pos
        onBoardInternal = x >= 0 && x < size && y >= 0 && y < size

possibleMoves :: Pos -> [Pos]
possibleMoves (x,y) = [ (x+1, y), (x, y+1), (x-1, y), (x, y-1) ]


data Tree v = Node v (Tree v) | Leaf v