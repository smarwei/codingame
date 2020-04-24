module Simulation.Data where

import qualified Data.Vector as V
import qualified Data.Sequence as S

data BoardEntity = SpawnPoint | Wall | Tavern | Mine | Air deriving (Show, Eq)

type Board = V.Vector (V.Vector BoardEntity)
type IndexedBoard = V.Vector (Pos, BoardEntity)

type Pos = (Int, Int)

-- (gold, life, hero pos, own mines)
type GameState = (Int, Int, Pos, V.Vector Pos)
