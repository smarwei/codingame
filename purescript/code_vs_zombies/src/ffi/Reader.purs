module Reader where

import Effect (Effect)

type Player =
    { x :: Int
    , y :: Int
    }

type Human =
    { id :: Int
    , x :: Int
    , y :: Int
    }

type Zombie =
    { id :: Int
    , x :: Int
    , y :: Int
    , nextX :: Int
    , nextY :: Int
    }

type GameInput =
    { player :: Player
    , humanCount :: Int
    , zombieCount :: Int
    , humans :: Array Human
    , zombies :: Array Zombie
    }

foreign import parseInput :: Effect GameInput

foreign import readline :: Effect String