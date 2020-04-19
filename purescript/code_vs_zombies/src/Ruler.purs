module Range
    ( Area(..)
    , Pos(..)
    , Range(..)
    , range
) where

import Prelude

-- data Building = Building Int Int
data Pos = Pos Int Int
data Area = Area Range Range

instance showPos :: Show Pos where
  show (Pos x y) = show x <> " " <> show y

instance showRange :: Show Range where
  show (Range x y) = show x <> "-" <> show y

instance showArea :: Show Area where
  show (Area r1 r2) = show r1 <> " / " <> show r2

data Range = Range Int Int

range :: Int -> Int -> Range
range x y = Range (min x y) (max x y)
