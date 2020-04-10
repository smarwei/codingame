module Main where

import Prelude

import Control.MonadZero (guard)
import Data.Array (any, filter, foldl, head, length, sortBy, (!!))
import Data.JSDate (now, getTime)
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Effect.Console (error, log)
import Effect.Random (randomInt)
import GameInput (Minion, Site, SiteInfo, ProtoSite, parseInitInput, parseInput)
import Lib (dist)
import Partial.Unsafe (unsafePartial)

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

    loop' numSites input.gold touchedSite (combinedSites input.sites) input.units leftSide'
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
                 }

loop' :: Int -> Int -> Maybe Int -> Array Site -> Array Minion -> Boolean -> Effect Unit
loop' numSites gold touchedSite sites units leftSide = do
    n <- now
    let t0 = getTime n

    log buildAll
    t <- trainAll gold sites
    log $ t

    n' <- now
    let t1 = getTime n
    -- error $ show t0 <> "Execution time: " <> (show $ t1 - t0)
    loop numSites (toSiteInfo <$> sites) (Just leftSide)
    where
        buildAll :: String
        buildAll =
            if (length $ friendlySites sites) == 0
                then buildTowers
            else if (length $ friendlyMines sites) < 3
                then buildMines
            else if (length $ friendlySites sites) > 5 && (length $ friendlySites sites) < 11
                then buildTowers
            else if (length $ friendlySites sites) <= 5
                then case head $ nearSites (queen units) sites of
                    Just site -> do
                        let typ = if hasKnightsBarrack sites then 1 else 0
                        build site typ
                    Nothing -> avoid
            else avoid

        buildTowers :: String
        buildTowers = case head $ nearSites (corner leftSide) sites of
            Just site -> "BUILD " <> show site.id <> " TOWER"
            Nothing -> avoid
        
        buildMines :: String
        buildMines = case head $ nearNonEmptyMines (queen units) sites of
            Just site -> "BUILD " <> show site.id <> " MINE"
            Nothing -> avoid

        avoid :: String
        avoid = case nearestEnemy of
            Just enemy -> "MOVE " <> show (site enemy).x <> " " <> show (site enemy).y
            Nothing -> "MOVE 0 0"
            where site enemy = unsafePartial $ fromJust $ head $
                            sortBy (\s1 s2 -> compare (dist enemy s2) (dist enemy s1)) (friendlySites sites)

        -- nearest non-queen enemy
        nearestEnemy :: Maybe Minion
        nearestEnemy = head $ filter (\u -> u.unitType /= -1 && isEnemy u) units

-- TODO: make pure
trainAll :: Int -> Array Site -> Effect String
trainAll gold sites = do
    randBarrack <- randomBarrack
    choose <- randomInt 1 100
    let barrack = if gold > 140
        then if choose < 42 then knightBarrack else archerBarrack
        else []
    pure $ foldl siteToIds "TRAIN" barrack
    where
        siteToIds acc site = acc <> " " <> show site.id
        knightBarrack = case head $ knightBarracks sites of
            Just barrack -> [barrack]
            Nothing -> []
        archerBarrack = case head $ archerBarracks sites of
            Just barrack -> [barrack]
            Nothing -> []
        randomBarrack = do
            let ownBarracks = filter isOwn $ barracks sites
            rand <- randomInt 0 $ length ownBarracks
            case ownBarracks !! rand of
                Just barrack -> pure [barrack]
                Nothing -> pure []

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

nearSites :: forall x. { x :: Int, y :: Int | x } -> Array Site -> Array Site
nearSites unit sites = sortBy (compareSiteDist unit) (freeSites sites)

nearNonEmptyMines :: forall x. { x :: Int, y :: Int | x } -> Array Site -> Array Site
nearNonEmptyMines unit sites = filter (\s -> s.gold > 0) $ nearSites unit sites

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
