module Simulation.Data where

import qualified Data.Vector as V
import qualified Data.Sequence as S

data BoardEntity = SpawnWanderer | Wall | Empty deriving (Show, Eq)

type Board = V.Vector (V.Vector BoardEntity)

type Pos = (Int, Int)

data EntityInput
    = ExplorerInput Int Pos Int Int
    | WandererInput Int Pos Int Int Int
    deriving (Show)

data Explorer = Explorer
    { explorerId :: Int
    , explorerPos :: Pos
    , explorerSanity :: Int
    , plansLeft :: Int
    }

data Wanderer = Wanderer
    { wandererId :: Int
    , wandererPos :: Pos
    , wandererRecallTime :: Int  -- time before recall
    , wandererStatus :: Int -- 0: spawning ; 1=wandering
    , wandererTarget :: Int 
    }

-- (hero, enemies)
type GameState = (Explorer, V.Vector Wanderer)