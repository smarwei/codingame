module Main where

-- mines only on side
-- build giants
-- Pattern match filed wei keine eigenes gebaeude
-- letzte Mine wird nciht aufgelevelt?

import Prelude

import Control.Monad.State (State, gets, modify_, runState)
import Control.MonadZero (empty, guard)
import Data.Array (any, concatMap, filter, foldl, head, length, sort, sortBy, (..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (fst, snd)
import Effect (Effect)
import Effect.Console (error, log)
import GameInput (Minion, Site, SiteInfo, ProtoSite, parseInitInput, parseInput)
import Graph as G
import Lib (dist)
import Partial.Unsafe (unsafePartial)

type GameState = 
    { gold :: Int
    , numSites :: Int
    , touchedSite :: Int
    , sites :: Array Site
    , units :: Array Minion
    , leftSide :: Boolean
    }

main :: Effect Unit
main = do
    initInput <- parseInitInput
    --error $ show $ concatMap nodeConnections nodes
    nextRound initInput.numSites initInput.sites Nothing G.empty
    where
         -- graph :: G.Graph String
         -- graph = unsafePartial $ fromJust $ foldl G.addNode graph' [ ["[1,1]", "[2,2]"], ["[3,4]", "[4,4]"], ["[2,2]", "[4,4]"] ]
         graph' = foldl G.addNode G.empty sNodes
         sNodes :: Array String
         sNodes = map (\n -> show n) nodes
         nodes = do
             x <- (1..4)
             y <- (1..4)
             pure $ [x, y]
         nodeConnections :: Array Int -> Array (Array String)
         nodeConnections [x, y] = [ [o, show [x-1,y]], [o, show [x+1,y]], [o, show [x,y-1]], [o, show [x,y+1]] ]
             where o = show [x, y]
         nodeConnections _ = []

nextRound :: Int -> Array SiteInfo -> Maybe GameState -> G.Graph String -> Effect Unit
nextRound numSites siteInfo gameState graph = do
    input <- parseInput numSites
    -- error $ show $ G.shortestPath "[4,4]" "[1,1]" graph

    -- do we start on the left side of the map?
    let leftSide' = case gameState of
            Just gs -> gs.leftSide
            Nothing -> (queen input.units).x < 500
    
    let gameState' =
            { gold: input.gold
            , numSites
            , touchedSite: input.touchedSite
            , sites: combinedSites input.sites
            , units: input.units
            , leftSide: leftSide'
            }

    let res = runState loop gameState'
    let state = snd res
    let val = fst res
    log $ val
    nextRound state.numSites (toSiteInfo <$> state.sites) (Just state) graph
    
    where
        -- combine sites with siteInfo and old state
        combinedSites :: Array ProtoSite -> Array Site
        combinedSites sites = do
            protoS <- sites
            infoS <- siteInfo
            guard $ protoS.id == infoS.id
            let prevSite = case gameState of
                    Just gs -> head $ filter (\s -> s.id == infoS.id) gs.sites
                    Nothing -> Nothing
            let lvl = case prevSite of
                    Just pSite -> if protoS.owner /= 0
                        then 0  -- reset level of all buildings, which are not (any longer) ours
                        else pSite.lvl
                    Nothing -> 0
            pure { id: protoS.id
                 , gold: protoS.gold
                 , maxMineSize: protoS.maxMineSize
                 , structureType: protoS.structureType
                 , owner: protoS.owner
                 , param1: protoS.param1
                 , param2: protoS.param2
                 , x: infoS.x
                 , y: infoS.y
                 , radius: infoS.radius
                 , lvl
                 }

loop :: State GameState String
loop = do
    ba <- buildAll
    ta <- trainAll
    pure $ ba <> "\n" <> ta

buildAll :: State GameState String
buildAll = do
    sites <- gets _.sites
    let buildingsCnt = length $ friendlySites sites
    let noTowerBuildingsCnt = length $ filter (not isTower) $ friendlySites sites
    let mines = length $ maxLvlFriendlyMines sites
    if mines < 4    -- TODO: letzte Mine muss auch ganz ausgebaut werden
        then buildMines
    else if noTowerBuildingsCnt < 5
        then buildBarracks
    else if buildingsCnt < 8 || (minTowerRange sites) < 300
        then if buildingsCnt < 8 then buildTowers
        else refreshTowers
    else if noTowerBuildingsCnt < 6
        then buildBarracks
    else refreshTowers
    where
        minTowerRange sites = case head $ sort $ map (\t -> t.param2) $ filter (isTower) $ friendlySites sites of
            Just t -> t
            Nothing -> 0

buildBarracks :: State GameState String
buildBarracks = do
    sites <- gets _.sites
    units <- gets _.units
    case head $ nearFreeSites (queen units) sites of
        Just site -> do
            let typ = --if not $ hasArcherBarrack sites then 1
                      if not $ hasKnightsBarrack sites then 0
                      else 2
            pure $ build site typ
        Nothing -> refreshTowers

buildTowers :: State GameState String
buildTowers = do
    leftSide <- gets _.leftSide
    units <- gets _.units
    sites <- gets _.sites
    case head $ nearFreeSites (queen units) sites of
        Just site -> pure $ "BUILD " <> show site.id <> " TOWER"
        Nothing -> refreshTowers

refreshTowers :: State GameState String
refreshTowers = do
    towers <-friendlyTowersByAttraction
    case head towers of
        Just site -> do
            touched <- gets _.touchedSite
            if touched == -1 || touched /= site.id
                then pure unit
                else modify_ (\s -> s { sites = map (incLvl site.id) s.sites })
            pure $ "BUILD " <> show site.id <> " TOWER"
        Nothing -> avoid
    where
        incLvl :: Int -> Site -> Site
        incLvl sId site
            | sId == site.id = site { lvl = site.lvl + 1 }
            | otherwise = site
    

buildMines :: State GameState String
buildMines = do
    units <- gets _.units
    sites <- gets _.sites
    leftSide <- gets _.leftSide
    case head $ nearNonEmptyMines (queen units) sites of
        Just site -> do
            touched <- gets _.touchedSite
            if touched == -1 || touched /= site.id
                then pure unit
                else modify_ (\s -> s { sites = map (incMineLvl site.id) s.sites })
            pure $ "BUILD " <> show site.id <> " MINE"
        Nothing -> avoid
    where
        incMineLvl :: Int -> Site -> Site
        incMineLvl sId site
            | sId == site.id = site { lvl = site.lvl + 1 }
            | otherwise = site

avoid :: State GameState String
avoid = do
    sites <- gets _.sites
    nEnemy <- nearestEnemy
    case nEnemy of
        Just enemy -> do
            let site = unsafePartial $ fromJust $ head $
                    sortBy (\s1 s2 -> compare (dist enemy s2) (dist enemy s1)) (friendlySites sites)
            pure $ moveToPos site
        Nothing -> pure $ "MOVE 0 0"

-- nearest non-queen enemy
nearestEnemy :: State GameState (Maybe Minion)
nearestEnemy = do
    units <- gets _.units
    pure $ head $ filter (\u -> isEnemy u) units

trainAll :: State GameState String
trainAll = do
    gold <- gets _.gold
    sites <- gets _.sites
    units <- gets _.units

    let ownGiants = filter isGiant $ ownMinions units
    let trainGiants = length (enemyTowers sites) > 2 && length ownGiants < 3
    let barrack = if not trainGiants
        then knightBarracks sites
        else if gold > 140 then
            --knightBarracks sites

            -- if length ownArchers < 4 && length (enemyKnights units) /= 0
            --     then archerBarrack sites
            giantBarrack sites
        else []
    pure $ foldl siteToIds "TRAIN" barrack
    where
        siteToIds acc site = acc <> " " <> show site.id
        knightBarrack sites = case head $ knightBarracks sites of
            Just barrack -> [barrack]
            Nothing -> []
        archerBarrack sites = case head $ archerBarracks sites of
            Just barrack -> [barrack]
            Nothing -> []
        giantBarrack sites = case head $ giantBarracks sites of
            Just barrack -> [barrack]
            Nothing -> knightBarracks sites

build :: forall e. { id :: Int | e } -> Int -> String
build s typ = "BUILD " <> show s.id <> " BARRACKS-" <> t
    where t | typ == 0 = "KNIGHT"
            | typ == 1 = "ARCHER"
            | otherwise = "GIANT"

queen :: Array Minion -> Minion
queen units = unsafePartial $ fromJust $ head $ filter (\u -> u.unitType == -1 && u.owner == 0) units

enemyQueen :: Array Minion -> Minion
enemyQueen units = unsafePartial $ fromJust $ head $ filter (\u -> u.unitType == -1 && u.owner == 1) units

ownMinions :: Array Minion -> Array Minion
ownMinions = filter isOwn

enemyKnights :: Array Minion -> Array Minion
enemyKnights = filter isEnemy <<< filter isKnight

freeSites :: Array Site -> Array Site
freeSites = filter (\s -> s.owner == -1)

friendlySites :: Array Site -> Array Site
friendlySites = filter (\s -> s.owner == 0)

friendlyMines :: Array Site -> Array Site
friendlyMines sites = filter (\s -> s.structureType == 0) $ friendlySites sites

maxLvlFriendlyMines :: Array Site -> Array Site
maxLvlFriendlyMines sites = filter (\s -> s.lvl >= s.maxMineSize) $ friendlyMines sites

enemyTowers :: Array Site -> Array Site
enemyTowers = filter isEnemy <<< filter isTower

friendlyTowers :: Array Site -> Array Site
friendlyTowers = filter isOwn <<< filter isTower

friendlyTowersByLvl :: Array Site -> Array Site
friendlyTowersByLvl sites = sortBy (\s1 s2 -> compare s1.lvl s2.lvl) (friendlyTowers sites)

-- TODO: Queen should use state -> easier
-- less distant towers and towers with less hp are preferred
friendlyTowersByAttraction :: State GameState (Array Site)
friendlyTowersByAttraction = do
    sites <- gets _.sites
    units <- gets _.units
    let q = queen units
    pure $ sortBy (\t1 t2 -> compare (attraction t1 q) (attraction t2 q)) (friendlyTowers sites)
    where attraction t q = t.param1 + dist t q

nearSites :: forall a. { x :: Int, y :: Int | a } -> Array Site -> Array Site
nearSites minion sites = sortBy (compareSiteDist minion) sites

nearFreeSites :: forall a. { x :: Int, y :: Int | a } -> Array Site -> Array Site
nearFreeSites minion sites = sortBy (compareSiteDist minion) (freeSites sites)

nearNonEmptyMines :: forall x. { x :: Int, y :: Int | x } -> Array Site -> Array Site
nearNonEmptyMines minion sites = filter (\s -> (s.gold > 20 || s.gold == -1) && s.lvl < 3 && s.owner /= 1) $ nearSites minion sites

hasKnightsBarrack :: Array Site -> Boolean
hasKnightsBarrack sites = any (\s -> s.param2 == 0) (friendlySites sites)

hasArcherBarrack :: Array Site -> Boolean
hasArcherBarrack sites = any (\s -> s.param2 == 1) (friendlySites sites)

hasGiantsBarrack :: Array Site -> Boolean
hasGiantsBarrack sites = any (\s -> s.param2 == 2) (friendlySites sites)

knightBarracks :: Array Site -> Array Site
knightBarracks sites = filter (\s -> s.param2 == 0) (friendlySites sites)

archerBarracks :: Array Site -> Array Site
archerBarracks sites = filter (\s -> s.param2 == 1) (friendlySites sites)

giantBarracks :: Array Site -> Array Site
giantBarracks sites = filter (\s -> s.param2 == 2) (friendlySites sites)

toSiteInfo :: Site -> SiteInfo
toSiteInfo s = { id: s.id, x: s.x, y: s.y, radius: s.radius }

compareSiteDist :: forall x. { x :: Int, y :: Int | x } -> Site -> Site -> Ordering
compareSiteDist u s1 s2 = compare (dist s1 u) (dist s2 u)

corner :: Boolean -> { x :: Int, y :: Int }
corner leftSide = if leftSide then { x: 0, y: 0 } else { x: 1920, y: 1000 }

isOwn :: forall a. { owner :: Int | a } -> Boolean
isOwn = owner 0

isEnemy :: forall a. { owner :: Int | a } -> Boolean
isEnemy = owner 1

owner :: Int -> forall a. { owner :: Int | a } -> Boolean
owner oId r = r.owner == oId

isKnight :: Minion -> Boolean
isKnight minion = minion.unitType == 0

isArcher :: Minion -> Boolean
isArcher minion = minion.unitType == 1

isGiant :: Minion -> Boolean
isGiant minion = minion.unitType == 2

isTower :: forall a. { structureType :: Int | a } -> Boolean
isTower s = s.structureType == 1

barracks :: Array Site -> Array Site
barracks sites = filter (\b -> b.structureType == 2) sites

moveToPos :: forall e. { x :: Int, y :: Int | e } -> String
moveToPos p = "MOVE " <> show p.x <> " " <> show p.y
