module Simulation.Board
    ( simulate
    ) where

-- import Prelude
import qualified Data.Vector as V
import Control.Monad.State as S
import Control.Monad.State.Class
import Data.List as L
import Simulation.Data

size = 10  -- TODO: Allow for variable board sizes
searchDepth = 5

-- fromPlayerBoard :: Board -> BoardInternal
-- fromPlayerBoard pBoardInternal = fmap (fmap $ fromEnum) asVector
--     where asVector = V.fromList $ fmap V.fromList pBoardInternal

emptyBoard :: Board
emptyBoard = V.generate 9 (\_ -> V.replicate 9 Air) 

-- All valid board positions are possible. For example the player could move
-- back and forth between two fields infinitely
-- Caution: if the player moved inside a Tavern or Mine he needs to be reset to his initial position afterwards
-- TODO: Check if tailrec
simulate :: Board -> Pos -> GameState -> (Int, Pos)
simulate board pos = evalState sim
    where sim = simulateMove board pos searchDepth (-1,-1)

simulateMove :: Board -> Pos -> Int -> Pos -> State GameState (Int, Pos)
simulateMove board pos depth prevPos
    | depth == 0 = do
        evalMove board pos
        gold <- evalGameState
        pure $ (gold, pos)
    | otherwise = do
        evalMove board pos
        let bPos = boardPos board pos
        let pos' = if bPos == Tavern || bPos == Mine then prevPos else pos      -- move back out of tavern/mine
        vals <- S.mapM (\pos'' -> simulateMove board pos'' (depth-1) pos') moves
        -- let valsWithPos = zip (fmap fst vals) moves   -- return poss of current move, not of submoves
        -- pure $ L.maximumBy (\(v1, _) (v2, _) -> compare v1 v2) valsWithPos
        let valsWithOldPos = if depth == searchDepth
                then vals   -- return position of submove on first level
                else zip (fmap fst vals) (replicate 4 pos')  -- return starting position otherwise
        pure $ L.maximumBy (\(v1, _) (v2, _) -> compare v1 v2) valsWithOldPos
        -- let maxVal = L.maximum valsWithOldPos
        -- pure (maxVal, if depth == searchDepth then else pos)
    where
        moves :: [Pos]
        moves = filter (posValid board) $ possibleMoves pos

-- update State according to hero position on board
-- executed every move
evalMove :: Board -> Pos -> State GameState ()
evalMove board pos
    | entity == Air = modify (\(gold, life, mines) -> (gold + length mines, life-1, mines))
    | entity == SpawnPoint = modify (\(gold, life, mines) -> (gold + length mines, life-1, mines))
    | entity == Tavern = modify ( \(gold, life, mines) -> (gold + length mines - 2, min 100 (life+50), mines) )  -- TODO: Check if life is +19
    | entity == Mine =
        modify (\(gold, life, mines) ->
            let addMine = pos `V.notElem` mines
                mines' = if addMine then V.cons pos mines else mines
            in
                ( gold + 1 + length mines'
                , life - 1
                , mines'
                ))
    | entity == Wall = pure () -- should never happen
    where
        entity = boardPos board pos

-- retuns the evalutaion of the current move
-- executed if maximum depth is reached
evalGameState :: State GameState Int
evalGameState = do
    (gold, _, _) <- get
    pure gold

-- get BoardInternalEntity Enum of Pos on BoardInternal
boardPos :: Board -> Pos -> BoardEntity
boardPos board (x,y) = (board V.! x) V.! y

posValid :: Board -> Pos -> Bool
posValid board pos@(x,y) = onBoardInternal && boardPos' /= Wall
    where
        boardPos' = boardPos board pos
        onBoardInternal = x >= 0 && x < size && y >= 0 && y < size

possibleMoves :: Pos -> [Pos]
possibleMoves (x,y) = [ (x+1, y), (x, y+1), (x-1, y), (x, y-1) ]


data Tree v = Node v (Tree v) | Leaf v

