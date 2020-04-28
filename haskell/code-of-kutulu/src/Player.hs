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
import Simulation.Lib
import Simulation.Board

runMain :: IO ()
runMain = runBot True bot

bot :: Bot
bot readLine writeLine = do
    input_line <- getLine
    let width = read input_line :: Int
    input_line <- getLine
    let height = read input_line :: Int

    board' <- V.replicateM height getLine
    let board :: Board = fmap (\br -> fmap (\se -> if
            | se == '.' -> Empty
            | se == '#' -> Wall
            | otherwise -> SpawnWanderer) $ V.fromList br) board' -- TODO: $ digitToInt se) br) board'
    
    input_line <- getLine
    let input = words input_line
    let sanitylosslonely = read (input!!0) :: Int -- how much sanity you lose every turn when alone, always 3 until wood 1
    let sanitylossgroup = read (input!!1) :: Int -- how much sanity you lose every turn when near another player, always 1 until wood 1
    let wandererspawntime = read (input!!2) :: Int -- how many turns the wanderer take to spawn, always 3 until wood 1
    let wandererlifetime = read (input!!3) :: Int -- how many turns the wanderer is on map after spawning, always 40 until wood 1
    
    -- game loop
    forever $ do
        input_line <- getLine
        let entitycount = read input_line :: Int -- the first given entity corresponds to your explorer
        
        entities <- V.replicateM entitycount $ do
            input_line <- getLine
            let input = words input_line
            let entitytype = input!!0
            let id = read (input!!1) :: Int
            let x = read (input!!2) :: Int
            let y = read (input!!3) :: Int
            let param0 = read (input!!4) :: Int
            let param1 = read (input!!5) :: Int
            let param2 = read (input!!6) :: Int
            pure $ if entitytype == "WANDERER"
                then WandererInput id (x,y) param0 param1 param2
                else ExplorerInput id (x,y) param0 param1
        
        let explorers' = fmap (\(ExplorerInput a b c d) -> Explorer a b c d) $ V.filter isExplorer entities
        let wanderers = fmap (\(WandererInput a b c d e) -> Wanderer a b c d e) $ V.filter (not . isExplorer) entities
        let hero = V.head explorers'
        let explorers = V.tail explorers'

        let cmd = if (all (> 6) $ fmap ((dist $ explorerPos hero) . wandererPos) wanderers) && explorerSanity hero `div` plansLeft hero < 100
            then "PLAN"
            else (\(_,(Explorer _ pos _ _, w)) -> moveToPos pos) $ simulate board (hero, wanderers)

        -- hPrint stderr $ minimum $ fmap (dist $ posFromEntity hero) (fmap posFromEntity eMines)
        putStrLn cmd

moveToPos :: Pos -> String
moveToPos (x, y) = "MOVE " <> (show x) <> " " <> (show y)

isExplorer :: EntityInput -> Bool
isExplorer e@(ExplorerInput _ _ _ _) = True
isExplorer _ = False