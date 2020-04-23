{-# LANGUAGE ScopedTypeVariables, LambdaCase, MultiWayIf #-}
module Player
    ( runMain
    , Board
    ) where

import System.IO
import Control.Monad
import System.Random
import Data.Char (digitToInt)
import Data.List as L
import qualified Data.Vector as V

import BotRunner
import Graph
import Simulation.Data
import Simulation.Board (simulate)


-- Id, Pos, life, gold
data Entity
    = EHero Int Pos Int Int 
    | EMine Int Pos

runMain :: IO ()
runMain = runBot True bot

bot :: Bot
bot readLine writeLine = do
    -- let graph = foldl addEdge' graph' $ concatMap nodeConnections nodes
    -- hPrint stderr $ shortestPathList graph "[0,0]" "[6,5]"

    input_line <- getLine
    let size = read input_line :: Int
    
    board' <- V.replicateM size getLine
    let board :: Board = fmap (\br -> fmap (\se -> if
            | se == '.' -> Air
            | se == '#' -> Wall
            | se == 'T' -> Tavern
            | se == 'M' -> Mine 
            | otherwise -> SpawnPoint) $ V.fromList br) board' -- TODO: $ digitToInt se) br) board'
    input_line <- getLine
    -- let iBoard :: IndexedBoard = Prelude.concatMap (\(i_r, br) -> fmap (\(i_c, bc) -> ((i_c, i_r), bc)) br) $ V.zip [0..9] $ fmap (V.zip [0..9]) board

    let myId = read input_line :: Int -- ID of your hero
    
    -- game loop
    forever $ do
        input_line <- getLine
        let entitycount = read input_line :: Int -- the number of entities
        
        entities <- V.replicateM entitycount $ do
            input_line <- getLine
            let input = words input_line
            let entitytype = input!!0 -- HERO or MINE
            let id = read (input!!1) :: Int -- the ID of a hero or the owner of a mine
            let x = read (input!!2) :: Int -- the x position of the entity
            let y = read (input!!3) :: Int -- the y position of the entity
            let life = read (input!!4) :: Int -- the life of a hero (-1 for mines)
            let gold = read (input!!5) :: Int -- the gold of a hero (-1 for mines)
            pure $ if entitytype == "HERO"
                then EHero id (x,y) life gold
                else EMine id (x,y)

        let heroes = V.filter (\e -> case e of
                EHero _ _ _ _ -> True
                _ -> False) entities
        let hero = V.head $ V.filter (\e -> case e of
                EHero id _ _ _ -> id == myId
                _ -> False) heroes
        -- let mines = V.filter (\e -> case e of
        --         EMine oId _ -> oId /= myId
        --         _ -> False) entities
        -- let minEMine = L.minimumBy (\e1 e2 -> compare (dist (posFromEntity e1) (posFromEntity hero)) (dist (posFromEntity e2) (posFromEntity hero))) mines
        -- let minTavernPos = L.minimumBy (\p1 p2 -> compare (dist p1 (posFromEntity hero)) (dist p2 (posFromEntity hero))) $ fmap (\(p, be) -> p) $ V.filter (\(p, be) -> isTavern be) iBoard

        let myMines = V.filter (\e -> case e of
                EMine oId _ -> oId == myId
                _ -> False) entities

        let (val, pos) = simulate board (posFromEntity hero) (gameState hero $ fmap posFromEntity myMines)
        -- hPrint stderr (gameState hero $ fmap posFromEntity myMines) -- (val, pos)
        putStrLn $ moveToPos pos
        
        -- putStrLn $ case life hero of
        --     Just lp -> if lp < 30 then moveToPos minTavernPos else moveToEntity minEMine
        --     Nothing -> moveToEntity minEMine

moveToEntity :: Entity -> String
moveToEntity e = case e of
    EHero _ p _ _ -> cout p
    EMine _ p -> cout p
    where cout (x,y) = "MOVE " <> (show x) <> " " <> (show y)

moveToPos :: (Int, Int) -> String
moveToPos (x, y) = "MOVE " <> (show x) <> " " <> (show y)

dist :: Pos -> Pos -> Int
dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

life :: Entity -> Maybe Int
life (EHero _ _ l _) = Just l
life _ = Nothing

posFromEntity :: Entity -> (Int, Int)
posFromEntity (EHero _ p _ _) = p
posFromEntity (EMine _ p) = p

gameState :: Entity -> V.Vector Pos -> GameState
gameState (EHero _ _ l g) mines = (g, l, mines)
gameState (EMine _ _) mines = (-1, -1, mines)

isTavern :: BoardEntity -> Bool
isTavern Tavern = True
isTavern _ = False

-- addEdge' :: Ord v => Graph v -> [v] -> Graph v
-- addEdge' g v = addEdge'' g v
--     where
--         addEdge'' :: Ord v => Graph v -> [v] -> Graph v
--         addEdge'' g [a,b] = addEdge g a b
-- 
-- graph' = foldl addNode empty sNodes
-- 
-- sNodes :: [String]
-- sNodes = map (\n -> show n) nodes
-- 
-- nodes = do
--     x <- [0..9]
--     y <- [0..9]
--     return [x, y]
-- 
-- nodeConnections :: [Int] -> [[String]]
-- nodeConnections [x, y] = [ [o, show [x-1,y]], [o, show [x+1,y]], [o, show [x,y-1]], [o, show [x,y+1]] ]
--     where o = show [x, y]
-- nodeConnections _ = []