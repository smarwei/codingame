module GameInput where

import Effect (Effect)

-- TODO: Convert to proper types

data BoardElement = Spawn Int | Wall | Tavern | Mine | Empty
type Board = Array (Array String)

type GameInitInput =
    { boardSize :: Int
    , heroId :: Int
    , board :: Board
    }

type Entity =
    { type :: String
    , id :: Int
    , x :: Int
    , y :: Int
    , life :: Int
    , gold :: Int
    }

type GameInput =
    { entityCount :: Int
    , entities :: Array Entity
    }

foreign import parseInitInput :: Effect GameInitInput

foreign import parseInput :: Effect GameInput

foreign import readline :: Effect String