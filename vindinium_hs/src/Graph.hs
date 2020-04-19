{-# LANGUAGE ScopedTypeVariables #-}
module Graph where

import Prelude
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import qualified Data.Sequence as Seq

newtype Graph v = Graph (M.Map v (Seq.Seq v))

empty :: forall v. Graph v
empty = Graph M.empty

addNode :: forall v. Ord v => Graph v -> v -> Graph v
addNode (Graph m) v = Graph $ M.insert v Seq.empty m

-- adds an Edge from Node "from" to Node "to"
-- returns the graph unmodified if "to" does not exist
addEdge :: forall v. Ord v => Graph v -> v -> v -> Graph v
addEdge g@(Graph m) from to = Graph $ M.update updateVal from m
    where
        updateVal :: Seq.Seq v -> Maybe (Seq.Seq v)
        updateVal nodes
            | g `contains` to = Just $ to Seq.<| nodes
            | otherwise = Just nodes

toMap :: forall v. Graph v -> M.Map v (Seq.Seq v)
toMap (Graph m) = m

adjacentEdges :: forall v. Ord v => Graph v -> v -> Seq.Seq v
adjacentEdges (Graph m) nodeId = fromMaybe Seq.empty $ M.lookup nodeId m

contains :: forall v. Ord v => Graph v -> v -> Bool
contains (Graph m) key = case M.lookup key m of
    Just _  -> True
    Nothing -> False

-- shortestPath :: forall v. Ord v => Graph v -> v -> v -> Seq v
-- shortestPath g@(Graph m) from to = reverse $ shortestPath' (from, Seq.empty) Seq.empty S.empty
--     where
--         shortestPath' :: (v, Seq v) -> [(v, Seq v)] -> S.Set v-> Seq v
--         shortestPath' from queue visited
--             | fst from == to = snd from
--             | length newQueue == 0 = Seq.empty
--             | otherwise = shortestPath' (head newQueue) newQueue (S.insert (fst from) visited)
--             where
--                 adjacent :: S.Set v
--                 adjacent = S.fromList $ adjacentEdges g (fst from)
--                 newQueue :: [(v, Seq v)]
--                 newQueue = drop 1 queue <> ( map (\x -> (x, fst from : snd from)) (S.toList $ S.difference adjacent visited) )

shortestPathList :: forall v. Ord v => Graph v -> v -> v -> Seq.Seq v
shortestPathList g@(Graph m) from to = Seq.reverse $ shortestPath' (from, Seq.empty) Seq.empty Seq.empty
    where
        shortestPath' :: (v, Seq.Seq v) -> Seq.Seq (v, Seq.Seq v) -> Seq.Seq v-> Seq.Seq v
        shortestPath' from queue visited
            | fst from == to = snd from
            | otherwise = case Seq.viewl newQueue of
                n Seq.:< _ -> shortestPath' n newQueue (fst from Seq.<| visited)
                Seq.EmptyL -> Seq.empty
            where
                adjacent :: Seq.Seq v
                adjacent = adjacentEdges g (fst from)
                newQueue :: Seq.Seq (v, Seq.Seq v)
                newQueue = Seq.drop 1 queue <> (fmap (\x -> (x, fst from Seq.<| snd from)) (adjacent `without` visited) )

without :: Eq a => Seq.Seq a -> Seq.Seq a -> Seq.Seq a
without seq1 seq2 = Seq.filter (\e -> all ((/=) e) seq2) seq1

with :: Eq a => Seq.Seq a -> Seq.Seq a -> Seq.Seq a
with seq1 seq2 = seq1 <> (seq2 `without` seq1)

-- pathExists :: forall v. Ord v => Graph v -> v -> v -> Bool
-- pathExists g@(Graph m) from to = shortestPath' from Seq.empty Seq.empty
--     where
--         shortestPath' :: v -> Seq v -> Seq v -> Bool
--         shortestPath' from queue visited
--             | from == to = True
--             | otherwise  = case head $ newQueue of
--                   Just n  -> shortestPath' n newQueue (from : visited)
--                   Nothing -> False
--             where
--                 adjacent :: Seq v
--                 adjacent = adjacentEdges g from
--                 newQueue :: Seq v
--                 newQueue = drop 1 queue <> (adjacent \\ visited)

dfs :: forall v. Ord v => Graph v -> v -> v -> Bool
dfs g@(Graph m) from to = dfs' g from to Seq.empty

dfs' :: forall v. Ord v => Graph v -> v -> v -> Seq.Seq v -> Bool
dfs' g@(Graph m) from to visited
    | from == to = True
    | otherwise = any ((==) True) $ subcalls (adjacent `without` visited)
    where
        subcalls = fmap (\f -> dfs' g f to visited')
        visited' = with visited adjacent
        adjacent = adjacentEdges g from