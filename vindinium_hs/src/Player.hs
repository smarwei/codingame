{-# LANGUAGE ScopedTypeVariables, LambdaCase, MultiWayIf #-}
module Player
    ( runMain
    ) where

import System.IO
import Control.Monad
import System.Random
import Data.Char (digitToInt)
import Data.List (minimumBy)

import BotRunner
import Graph

data BoardEntity = BSpawnPoint Int | BWall | BTavern | BMine | BEmpty deriving (Show, Eq)
type Board = [[BoardEntity]]
type IndexedBoard = [(Pos, BoardEntity)]
type Pos = (Int, Int)

-- Id, Pos, life, gold
data Entity
    = Hero Int Pos Int Int 
    | Mine Int Pos

runMain :: IO ()
runMain = runBot True bot

bot :: Bot
bot readLine writeLine = do
    -- let graph = foldl addEdge' graph' $ concatMap nodeConnections nodes
    -- hPrint stderr $ shortestPathList graph "[0,0]" "[6,5]"

    input_line <- getLine
    let size = read input_line :: Int
    
    board' <- replicateM size getLine
    let board :: Board = map (\br -> map (\se -> if
            | se == '.' -> BEmpty
            | se == '#' -> BWall
            | se == 'T' -> BTavern
            | se == 'M' -> BMine 
            | otherwise -> BSpawnPoint $ digitToInt se) br) board'
    input_line <- getLine
    let iBoard :: IndexedBoard = concatMap (\(i_r, br) -> map (\(i_c, bc) -> ((i_c, i_r), bc)) br) $ zip [0..9] $ map (zip [0..9]) board

    let myId = read input_line :: Int -- ID of your hero
    
    -- game loop
    forever $ do
        input_line <- getLine
        let entitycount = read input_line :: Int -- the number of entities
        
        entities <- replicateM entitycount $ do
            input_line <- getLine
            let input = words input_line
            let entitytype = input!!0 -- HERO or MINE
            let id = read (input!!1) :: Int -- the ID of a hero or the owner of a mine
            let x = read (input!!2) :: Int -- the x position of the entity
            let y = read (input!!3) :: Int -- the y position of the entity
            let life = read (input!!4) :: Int -- the life of a hero (-1 for mines)
            let gold = read (input!!5) :: Int -- the gold of a hero (-1 for mines)
            pure $ if entitytype == "HERO"
                then Hero id (x,y) life gold
                else Mine id (x,y)

        let heroes = filter (\e -> case e of
                Hero _ _ _ _ -> True
                _ -> False) entities
        let hero = head $ filter (\e -> case e of
                Hero id _ _ _ -> id == myId
                _ -> False) heroes
        let mines = filter (\e -> case e of
                Mine oId _ -> oId /= myId
                _ -> False) entities
        let minMine = minimumBy (\e1 e2 -> compare (dist (posFromEntity e1) (posFromEntity hero)) (dist (posFromEntity e2) (posFromEntity hero))) mines
        let minTavernPos = minimumBy (\p1 p2 -> compare (dist p1 (posFromEntity hero)) (dist p2 (posFromEntity hero))) $ map (\(p, be) -> p) $ filter (\(p, be) -> isTavern be) iBoard
        -- hPrint stderr minMine
        
        -- WAIT | NORTH | EAST | SOUTH | WEST
        r <- randomRIO (0,3) :: IO Int
        let dir = if r == 0
                then "NORTH"
            else if r == 1
                then "EAST"
            else if r == 2
                then "SOUTH"
            else 
                "WEST"

        putStrLn $ case life hero of
            Just lp -> if lp < 30 then moveToPos minTavernPos else moveToEntity minMine
            Nothing -> moveToEntity minMine

moveToEntity :: Entity -> String
moveToEntity e = case e of
    Hero _ p _ _ -> cout p
    Mine _ p -> cout p
    where cout (x,y) = "MOVE " <> (show x) <> " " <> (show y)

moveToPos :: (Int, Int) -> String
moveToPos (x, y) = "MOVE " <> (show x) <> " " <> (show y)

dist :: Pos -> Pos -> Int
dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

life :: Entity -> Maybe Int
life (Hero _ _ l _) = Just l
life _ = Nothing

posFromEntity :: Entity -> (Int, Int)
posFromEntity (Hero _ p _ _) = p
posFromEntity (Mine _ p) = p

isTavern :: BoardEntity -> Bool
isTavern BTavern = True
isTavern _ = False

addEdge' :: Ord v => Graph v -> [v] -> Graph v
addEdge' g v = addEdge'' g v
    where
        addEdge'' :: Ord v => Graph v -> [v] -> Graph v
        addEdge'' g [a,b] = addEdge g a b

graph' = foldl addNode empty sNodes

sNodes :: [String]
sNodes = map (\n -> show n) nodes

nodes = do
    x <- [0..9]
    y <- [0..9]
    return [x, y]

nodeConnections :: [Int] -> [[String]]
nodeConnections [x, y] = [ [o, show [x-1,y]], [o, show [x+1,y]], [o, show [x,y-1]], [o, show [x,y+1]] ]
    where o = show [x, y]
nodeConnections _ = []