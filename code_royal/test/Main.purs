module Test.Main where

import Prelude

import Data.Array (concatMap, (..))
import Data.Foldable (foldl)
import Data.Int (fromNumber)
import Data.JSDate (JSDate, getTime, now)
import Data.Map (Map, showTree)
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Console (log)
import Graph (Graph(..), addEdge, addNode, dfs, empty, pathExists, shortestPath, shortestPathList, toMap, (<+>))
import Partial.Unsafe (unsafePartial)


main :: Effect Unit
main = do
    let graph = foldl addEdge' graph' $ concatMap nodeConnections nodes

    d0 <- now
    -- log $ show $ shortestPathList graph "[1,1]" "[7,7]"
    -- log $ show $ shortestPathList graph "[1,1]" "[7,7]"
    -- log $ show $ shortestPathList graph "[1,1]" "[7,7]"
    -- d1 <- now
    -- test "list search" d0 d1

    -- log ""

    -- d2 <- now
    -- log $ show $ shortestPath graph "[1,1]" "[7,7]"
    -- log $ show $ shortestPath graph "[1,1]" "[7,7]"
    -- log $ show $ shortestPath graph "[1,1]" "[7,7]"
    -- d3 <- now
    -- test "set search" d2 d3

    -- log ""

    -- d4 <- now
    -- log $ show $ pathExists graph "[1,1]" "[7,7]"
    -- log $ show $ pathExists graph "[1,1]" "[7,7]"
    -- log $ show $ pathExists graph "[1,1]" "[7,7]"
    -- d5 <- now
    -- test "exists test" d4 d5

    -- log ""

    d6 <- now
    log $ show $ dfs graph "[1,1]" "[1,2]"
    log $ show $ dfs graph "[1,1]" "[1,2]"
    log $ show $ dfs graph "[1,1]" "[1,2]"
    d7 <- now
    test "dfs test" d6 d7

    log ""

    log $ "execution time of ALL: " <> (show $ (getTime d7 - getTime d0) / 3000.0) <> "s"

test :: String -> JSDate -> JSDate -> Effect Unit
test tName d0 d1 = do
    let t0 = getTime d0
    let t1 = getTime d1
    log $ "execution time of " <> tName <> ": " <> (show $ (t1 - t0) / 3000.0) <> "s"

addEdge' :: forall v. Ord v => Graph v -> Array v -> Graph v
addEdge' g v = unsafePartial $ addEdge'' v
    where
        addEdge'' :: Partial => Array v -> Graph v
        addEdge'' [a,b] = addEdge g a b

graph' = foldl addNode empty sNodes

sNodes :: Array String
sNodes = map (\n -> show n) nodes

nodes = do
    x <- (1..9)
    y <- (1..9)
    pure $ [x, y]

nodeConnections :: Array Int -> Array (Array String)
nodeConnections [x, y] = [ [o, show [x-1,y]], [o, show [x+1,y]], [o, show [x,y-1]], [o, show [x,y+1]] ]
    where o = show [x, y]
nodeConnections _ = []