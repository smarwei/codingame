module Test.Main where

import Prelude

import Data.Array (concatMap, (..))
import Data.Foldable (foldl)
import Data.Int (fromNumber)
import Data.JSDate (getTime, now)
import Data.List (List)
import Data.Map (Map, showTree)
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Console (log)
import Graph (Graph(..), addEdge, addNode, empty, shortestPath, toMap, (<+>))
import Partial.Unsafe (unsafePartial)


main :: Effect Unit
main = do
    test "graph" testCreateGraph
    let f2 = log $ show $ shortestPath graph "[1,1]" "[8,8]"
    test "search" f2

--testCreateGraph :: forall v. Effect (Map v (List v))
testCreateGraph = pure $ toMap $ graph

test :: forall a. String -> Effect a -> Effect Unit
test tName fn = do
    d0 <- now
    let t0 = getTime d0
    _ <- fn
    d1 <- now
    log $ "execution time of " <> tName <> ": " <> (show $ unsafePartial $ fromJust $ fromNumber $ getTime d1 - t0) <> "ms"

graph :: Graph String
-- graph = foldl addEdge' graph' [ ["[1,1]", "[2,2]"], ["[3,4]", "[4,4]"], ["[2,2]", "[4,4]"] ]
graph = foldl addEdge' graph' $ concatMap nodeConnections nodes

addEdge' :: forall v. Ord v => Graph v -> Array v -> Graph v
addEdge' g v = unsafePartial $ addEdge'' v
    where
        addEdge'' :: Partial => Array v -> Graph v
        addEdge'' [a,b] = addEdge g a b

graph' = foldl addNode empty sNodes

sNodes :: Array String
sNodes = map (\n -> show n) nodes

nodes = do
    x <- (1..360)
    y <- (1..250)
    pure $ [x, y]

nodeConnections :: Array Int -> Array (Array String)
nodeConnections [x, y] = [ [o, show [x-1,y]], [o, show [x+1,y]], [o, show [x,y-1]], [o, show [x,y+1]] ]
    where o = show [x, y]
nodeConnections _ = []