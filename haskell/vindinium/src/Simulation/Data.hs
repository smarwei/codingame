module Simulation.Data where

import qualified Data.Vector as V

data BoardEntity = SpawnPoint | Wall | Tavern | Mine | Air deriving (Show, Eq, Enum)
type BoardEntityEnum = Int

type Board = [[BoardEntity]]
type IndexedBoard = [(Pos, BoardEntity)]

type BoardInternal = V.Vector (V.Vector BoardEntityEnum)
type Pos = (Int, Int)

-- (gold, life, numMines)
type GameState = (Int, Int, Int)
