module GameInput where

import Effect (Effect)

type GameInitInput =
    { numSites :: Int
    , sites :: Array SiteInfo
    }

type GameInput =
    { gold :: Int
    , touchedSite :: Int -- -1 if none
    , sites :: Array ProtoSite
    , units :: Array Minion
    }

type SiteInfo = 
    { id :: Int
    , x :: Int
    , y :: Int
    , radius :: Int
    }

type ProtoSite = 
    { id :: Int
    , structureType :: Int  -- -1 No structure, 2 Barracks
    , owner :: Int  -- -1 No structure, 0 friendly, 1 enemy
    , param1 :: Int  -- -1 No structure, else turns till training
    , param2 :: Int  -- -1 No structure, barracks: 0 knight 1 archer
    }

type Site =
    { id :: Int
    , x :: Int
    , y :: Int
    , radius :: Int
    , structureType :: Int
    , owner :: Int
    , param1 :: Int
    , param2 :: Int
    }

type Minion =
    { x :: Int
    , y :: Int
    , owner :: Int -- 0 = Friendly; 1 = Enemy
    , unitType :: Int -- -1 = QUEEN, 0 = KNIGHT, 1 = ARCHER
    , health :: Int
    }

foreign import parseInitInput :: Effect GameInitInput

foreign import parseInput :: Int -> Effect GameInput

foreign import readline :: Effect String