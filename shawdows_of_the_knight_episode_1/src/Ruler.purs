module Range
    ( Area(..)
    , Pos(..)
    , Range(..)
    , range
    , fromPos
) where

import Prelude

-- data Building = Building Int Int
data Pos = Pos Int Int
data Area = Area Range Range

instance showPos :: Show Pos where
  show (Pos x y) = show x <> " " <> show y

fromPos :: Pos -> Range
fromPos (Pos x y) = range x y

data Range = Range Int Int

range :: Int -> Int -> Range
range x y = Range (min x y) (max x y)
