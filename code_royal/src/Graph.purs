module Graph where

import Prelude

import Data.List (List(..), drop, head, reverse, (:), fromFoldable, (\\))
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as S
import Data.Tuple (Tuple(..), fst, snd)

newtype Graph v = Graph (M.Map v (List v))

empty :: forall v. Graph v
empty = Graph M.empty

addNode :: forall v. Ord v => Graph v -> v -> Graph v
addNode (Graph m) v = Graph $ M.insert v Nil m
infixl 5 addNode as <+>

-- adds an Edge from Node "from" to Node "to"
-- returns the graph unmodified if "to" does not exist
addEdge :: forall v. Ord v => Graph v -> v -> v -> Graph v
addEdge g@(Graph m) from to = Graph $ M.update updateVal from m
    where
        updateVal :: List v -> Maybe (List v)
        updateVal nodes
            | g `contains` to = Just $ to : nodes
            | otherwise = Just nodes

toMap :: forall v. Graph v -> M.Map v (List v)
toMap (Graph m) = m

adjacentEdges :: forall v. Ord v => Graph v -> v -> List v
adjacentEdges (Graph m) nodeId = fromMaybe Nil $ M.lookup nodeId m

contains :: forall v. Ord v => Graph v -> v -> Boolean
contains (Graph m) key = case M.lookup key m of
    Just _  -> true
    Nothing -> false

shortestPath :: forall v. Ord v => Graph v -> v -> v -> List v
shortestPath g@(Graph m) from to = reverse $ shortestPath' (Tuple from Nil) Nil S.empty
    where
        shortestPath' :: (Tuple v (List v)) -> List (Tuple v (List v)) -> S.Set v-> List v
        shortestPath' from queue visited
            | fst from == to = snd from
            | otherwise  = case head $ newQueue of
                  Just n  -> shortestPath' n newQueue (S.insert (fst from) visited)
                  Nothing -> Nil
            where
                adjacent :: S.Set v
                adjacent = S.fromFoldable $ adjacentEdges g (fst from)
                newQueue :: List (Tuple v (List v))
                newQueue = drop 1 queue <> ( map (\x -> Tuple x $ fst from : snd from) (fromFoldable $ S.difference adjacent visited) )