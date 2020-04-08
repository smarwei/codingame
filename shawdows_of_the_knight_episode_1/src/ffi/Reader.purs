module Reader where

import Effect (Effect)

type GameInput =
    { width :: Int
    , height :: Int
    , turns :: Int
    , x :: Int
    , y :: Int
    }

foreign import parseInput :: Effect GameInput

foreign import readline :: Effect String