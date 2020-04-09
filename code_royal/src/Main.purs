module Main where

import Prelude

import Data.Array (any, drop, filter, head, length, sortBy)
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Effect.Console (log, error)
import GameInput (Site, Minion, SiteInfo, parseInitInput, parseInput)
import Lib (dist, toPos)
import Partial.Unsafe (unsafePartial)
import Range (Pos(..))

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

    loop' numSites input.gold touchedSite siteInfo input.sites input.units

loop' :: Int -> Int -> Maybe Int -> Array SiteInfo -> Array Site -> Array Minion -> Effect Unit
loop' numSites gold touchedSite siteInfo sites units = do
    error $ "Sites: " <> show sites
    error $ "SiteI: " <> show siteInfo

    let queen = unsafePartial $ fromJust $ head $ filter (\u -> u.unitType == -1 && u.owner == 0) units
    let siteInfo' = sortBy (compareSiteInfoDist queen) siteInfo

    case head siteInfo' of
        Just sInfo -> do
            log $ build sInfo
            log $ "TRAIN " <> show sInfo.id
        Nothing -> do
            log $ "WAIT"
            log $ "TRAIN" 

    loop numSites siteInfo

compareSiteInfoDist :: Minion -> SiteInfo -> SiteInfo -> Ordering
compareSiteInfoDist u s1 s2 = compare (dist s1 u) (dist s2 u)

build :: forall e. { id :: Int | e } -> String
build s = "BUILD " <> show s.id <> " BARRACKS-ARCHER"

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