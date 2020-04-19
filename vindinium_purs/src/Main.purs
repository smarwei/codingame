module Main where

import Prelude

import Control.Monad.State (State, gets, modify_, runState)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Effect (Effect)
import Effect.Console (log, error)
import Effect.Random (randomInt)
import GameInput (parseInitInput, parseInput, GameInitInput, Board, Entity)
import Graph

type GameState = 
    { boardSize :: Int
    , heroId :: Int
    , board :: Board
    , entityCount :: Int
    , entities :: Array Entity
    }

main :: Effect Unit
main = do
    initInput <- parseInitInput
    error $ show $ initInput.board
    nextRound initInput Nothing

nextRound :: GameInitInput -> Maybe GameState -> Effect Unit
nextRound initInput gameState = do
    input <- parseInput
    -- error $ show $ G.shortestPath "[4,4]" "[1,1]" graph

    -- do we start on the left side of the map?
    
    let gameState' =
            { boardSize: initInput.boardSize
            , heroId: initInput.heroId
            , board: initInput.board
            , entityCount: input.entityCount
            , entities: input.entities
            }
    rand <- randomInt 0 3

    let res = runState (loop rand) gameState'
    let state = snd res
    let val = fst res
    log $ val
    nextRound initInput (Just state)

loop :: Int -> State GameState String
loop rand
    | rand == 0 = pure "NORTH"
    | rand == 1 = pure "EAST"
    | rand == 2 = pure "SOUTH"
    | otherwise = pure "WEST"
