module Main where

import Prelude

import Control.MonadZero (guard)
import Data.Array (any, filter, foldl, head, length, reverse, sortBy, (!!))
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Effect.Console (log, error)
import Effect.Random (randomInt)
import GameInput (Minion, Site, SiteInfo, ProtoSite, parseInitInput, parseInput)
import Lib (dist)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = do
    initInput <- parseInitInput
    error $ show initInput
    loop initInput.numSites initInput.sites

loop :: Int -> Array SiteInfo -> Effect Unit
loop numSites siteInfo = do
    input <- parseInput numSites
    
    let touchedSite = if input.touchedSite == -1
        then Nothing
        else Just input.touchedSite

    loop' numSites input.gold touchedSite (combinedSites input.sites) input.units
    where
        -- combine sites with siteInfo
        combinedSites :: Array ProtoSite -> Array Site
        combinedSites sites = do
            protoS <- sites
            infoS <- siteInfo
            guard $ protoS.id == infoS.id
            pure { id: protoS.id
                 , structureType: protoS.structureType
                 , owner: protoS.owner
                 , param1: protoS.param1
                 , param2: protoS.param2
                 , x: infoS.x
                 , y: infoS.y
                 , radius: infoS.radius
                 }

loop' :: Int -> Int -> Maybe Int -> Array Site -> Array Minion -> Effect Unit
loop' numSites gold touchedSite sites units = do
    -- error $ "Free sites: " <> (show $ map (\s -> s.id) freeSites)
    -- error $ "Near sites: " <> (show $ map (\s -> s.id) nearSites)

    if (length $ friendlySites sites) > 3
        then log avoid
        else case head $ nearSites queen sites of
            Just sInfo -> do
                let typ = if hasKnightsBarrack sites then 1 else 0
                log $ build sInfo typ
            Nothing -> do
                log avoid

    t <- trainAll gold sites
    log $ t

    loop numSites (toSiteInfo <$> sites)
    where
        avoid :: String
        avoid = case nearestEnemy of
            Just enemy -> "MOVE " <> show (site enemy).x <> " " <> show (site enemy).y
            Nothing -> "MOVE 0 0"
            where site enemy = unsafePartial $ fromJust $ head $
                            sortBy (\s1 s2 -> compare (dist enemy s2) (dist enemy s1)) (friendlySites sites)

        queen :: Minion
        queen = unsafePartial $ fromJust $ head $ filter (\u -> u.unitType == -1 && u.owner == 0) units

        enemyQueen :: Minion
        enemyQueen = unsafePartial $ fromJust $ head $ filter (\u -> u.unitType == -1 && u.owner == 1) units

        -- nearest non-queen enemy
        nearestEnemy :: Maybe Minion
        nearestEnemy = head $ filter (\u -> u.unitType /= -1 && isEnemy u) units

-- TODO: make pure
trainAll :: Int -> Array Site -> Effect String
trainAll gold sites = do
    randBarrack <- randomBarrack
    choose <- randomInt 1 100
    let barrack = if gold > 100 && choose < 23 then knightBarrack else randBarrack
    pure $ foldl siteToIds "TRAIN" barrack
    where
        siteToIds acc site = acc <> " " <> show site.id
        knightBarrack = case head $ knightBarracks sites of
            Just barrack -> [barrack]
            Nothing -> []
        randomBarrack = do
            let ownBarracks = filter isOwn $ barracks sites
            rand <- randomInt 0 $ length ownBarracks
            case ownBarracks !! rand of
                Just barrack -> pure [barrack]
                Nothing -> pure []

freeSites :: Array Site -> Array Site
freeSites = filter (\s -> s.owner == -1)

friendlySites :: Array Site -> Array Site
friendlySites = filter (\s -> s.owner == 0)

nearSites :: Minion -> Array Site -> Array Site
nearSites unit sites = sortBy (compareSiteInfoDist unit) (freeSites sites)

hasKnightsBarrack :: Array Site -> Boolean
hasKnightsBarrack sites = any (\s -> s.param2 == 0) (friendlySites sites)

knightBarracks :: Array Site -> Array Site
knightBarracks sites = filter (\s -> s.param2 == 0) (friendlySites sites)

toSiteInfo :: Site -> SiteInfo
toSiteInfo s = { id: s.id, x: s.x, y: s.y, radius: s.radius }

compareSiteInfoDist :: Minion -> Site -> Site -> Ordering
compareSiteInfoDist u s1 s2 = compare (dist s1 u) (dist s2 u)

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

-- Phase 1
--   Queen takes as much buildings as possible
--   Build Archers only
-- Phase 2
--   Queen destroys enemy buildings
--   Build Archers only
-- Phase 3
--   Queen avoids Knights
--   All Buildings but one train Archers

-- nearestBuilding :: Pos