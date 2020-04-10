module Main where

import Prelude

import Control.Monad.State (State, gets, runState)
import Control.MonadZero (guard)
import Data.Array (any, filter, foldl, head, length, sortBy)
-- import Data.JSDate (now, getTime)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (fst, snd)
import Effect (Effect)
import Effect.Console (error, log)
import GameInput (Minion, Site, SiteInfo, ProtoSite, parseInitInput, parseInput)
import Lib (dist)
import Partial.Unsafe (unsafePartial)

type GameState = 
    { gold :: Int
    , numSites :: Int
    , touchedSite :: Maybe Int
    , sites :: Array Site
    , units :: Array Minion
    , leftSide :: Boolean
    }

main :: Effect Unit
main = do
    initInput <- parseInitInput
    error $ show initInput
    loop initInput.numSites initInput.sites Nothing

loop :: Int -> Array SiteInfo -> Maybe Boolean -> Effect Unit
loop numSites siteInfo leftSide = do
    input <- parseInput numSites

    -- do we start on the left side of the map?
    let leftSide' = case leftSide of
            Just ls -> ls
            Nothing -> (queen input.units).x < 500
    
    let touchedSite = if input.touchedSite == -1
        then Nothing
        else Just input.touchedSite
    
    let gameState =
            { gold: input.gold
            , numSites
            , touchedSite
            , sites: combinedSites input.sites
            , units: input.units
            , leftSide: leftSide'
            }

    let res = runState loop' gameState
    let state = snd res
    let val = fst res
    log $ val
    loop state.numSites (toSiteInfo <$> state.sites) (Just state.leftSide)
    
    where
        -- queenPos :: Array Minion -> { x :: Int, y :: Int }
        -- queenPos minions = queen minions

        -- combine sites with siteInfo
        combinedSites :: Array ProtoSite -> Array Site
        combinedSites sites = do
            protoS <- sites
            infoS <- siteInfo
            guard $ protoS.id == infoS.id
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
                 , mineLvl: -1
                 }

loop' :: State GameState String
loop' = do
    ba <- buildAll
    ta <- trainAll
    pure $ ba <> "\n" <> ta

buildAll :: State GameState String
buildAll = do
    sites <- gets _.sites
    units <- gets _.units
    if (length $ friendlySites sites) == 0
        then buildTowers
    else if (length $ friendlyMines sites) < 3
        then buildMines
    else if (length $ friendlySites sites) > 5 && (length $ friendlySites sites) < 11
        then buildTowers
    else if (length $ friendlySites sites) <= 5
        then case head $ nearFreeSites (queen units) sites of
            Just site -> do
                let typ = if hasKnightsBarrack sites then 1 else 0
                pure $ build site typ
            Nothing -> avoid
    else avoid

buildTowers :: State GameState String
buildTowers = do
    leftSide <- gets _.leftSide
    sites <- gets _.sites
    case head $ nearFreeSites (corner leftSide) sites of
        Just site -> pure $ "BUILD " <> show site.id <> " TOWER"
        Nothing -> avoid

buildMines :: State GameState String
buildMines = do
    units <- gets _.units
    sites <- gets _.sites
    case head $ nearNonEmptyMines (queen units) sites of
        Just site ->
            pure $ "BUILD " <> show site.id <> " MINE"
        Nothing -> avoid

avoid :: State GameState String
avoid = do
    sites <- gets _.sites
    nEnemy <- nearestEnemy
    case nEnemy of
        Just enemy -> do
            let site = unsafePartial $ fromJust $ head $
                    sortBy (\s1 s2 -> compare (dist enemy s2) (dist enemy s1)) (friendlySites sites)
            pure $ "MOVE " <> show site.x <> " " <> show site.y
        Nothing -> pure $ "MOVE 0 0"

-- nearest non-queen enemy
nearestEnemy :: State GameState (Maybe Minion)
nearestEnemy = do
    units <- gets _.units
    pure $ head $ filter (\u -> u.unitType /= -1 && isEnemy u) units

trainAll :: State GameState String
trainAll = do
    gold <- gets _.gold
    sites <- gets _.sites

    -- choose <- randomInt 1 100
    let choose = -1   -- TODO!!!
    let barrack = if gold > 140
        then if choose < 42 then knightBarrack sites else archerBarrack sites
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

queen :: Array Minion -> Minion
queen units = unsafePartial $ fromJust $ head $ filter (\u -> u.unitType == -1 && u.owner == 0) units

enemyQueen :: Array Minion -> Minion
enemyQueen units = unsafePartial $ fromJust $ head $ filter (\u -> u.unitType == -1 && u.owner == 1) units

freeSites :: Array Site -> Array Site
freeSites = filter (\s -> s.owner == -1)

friendlySites :: Array Site -> Array Site
friendlySites = filter (\s -> s.owner == 0)

friendlyMines :: Array Site -> Array Site
friendlyMines sites = filter (\s -> s.structureType == 0) $ friendlySites sites

nearSites :: forall a. { x :: Int, y :: Int | a } -> Array Site -> Array Site
nearSites unit sites = sortBy (compareSiteDist unit) sites

nearFreeSites :: forall a. { x :: Int, y :: Int | a } -> Array Site -> Array Site
nearFreeSites unit sites = sortBy (compareSiteDist unit) (freeSites sites)

nearNonEmptyMines :: forall x. { x :: Int, y :: Int | x } -> Array Site -> Array Site
nearNonEmptyMines unit sites = filter (\s -> s.gold > 0 && s.mineLvl < 5 && s.owner /= 1) $ nearSites unit sites

hasKnightsBarrack :: Array Site -> Boolean
hasKnightsBarrack sites = any (\s -> s.param2 == 0) (friendlySites sites)

knightBarracks :: Array Site -> Array Site
knightBarracks sites = filter (\s -> s.param2 == 0) (friendlySites sites)

archerBarracks :: Array Site -> Array Site
archerBarracks sites = filter (\s -> s.param2 == 1) (friendlySites sites)

toSiteInfo :: Site -> SiteInfo
toSiteInfo s = { id: s.id, x: s.x, y: s.y, radius: s.radius }

compareSiteDist :: forall x. { x :: Int, y :: Int | x } -> Site -> Site -> Ordering
compareSiteDist u s1 s2 = compare (dist s1 u) (dist s2 u)

corner :: Boolean -> { x :: Int, y :: Int }
corner leftSide = if leftSide then { x: 0, y: 0 } else { x: 1920, y: 1000 }

build :: forall e. { id :: Int | e } -> Int -> String
build s typ = "BUILD " <> show s.id <> " BARRACKS-" <> t
    where t = if typ == 0 then "KNIGHT" else "ARCHER"

isOwn :: forall a. { owner :: Int | a } -> Boolean
isOwn = owner 0

isEnemy :: forall a. { owner :: Int | a } -> Boolean
isEnemy = owner 1

owner :: Int -> forall a. { owner :: Int | a } -> Boolean
owner oId r = r.owner == oId

barracks :: Array Site -> Array Site
barracks sites = filter (\b -> b.structureType == 2) sites

moveToPos :: forall e. { x :: Int, y :: Int | e } -> String
moveToPos p = "MOVE " <> show p.x <> " " <> show p.y
