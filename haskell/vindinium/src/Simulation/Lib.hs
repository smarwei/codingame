module Simulation.Lib where

import Simulation.Data

dist :: Pos -> Pos -> Int
dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)