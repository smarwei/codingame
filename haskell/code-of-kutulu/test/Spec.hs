import Simulation.Board
import Simulation.Data

main :: IO ()
main = putStrLn "Test suite not yet implemented"

emptyBoard :: Board
emptyBoard = V.generate 9 (\_ -> V.replicate 9 Air) 
