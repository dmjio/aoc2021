{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad   (guard)
import           Data.Char       (isLower)
import           Data.List.Split (splitOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (isNothing)
import           Data.Set        (Set)
import qualified Data.Set        as S

main :: IO ()
main = do
  ls <- fmap parse . lines <$> readFile "input.txt"
  let adjacencyList = mkAdjList ls
  part1 adjacencyList
  part2 adjacencyList

part1 :: AdjList -> IO ()
part1 adj
  = print
  $ length
  $ filter (\xs -> last xs == End && head xs == Start)
  $ findPath1 adj mempty Start

part2 :: AdjList -> IO ()
part2 adj
  = print
  $ length
  $ filter (\xs -> last xs == End && head xs == Start)
  $ findPath2 adj mempty Start

findPath2 :: AdjList -> Map Node Int -> Node -> [[Node]]
findPath2 m seen n = do
  let
    neighbors = S.toList (m M.! n)
    seen' = M.insertWith (+) n 1 seen
  case n of
    End ->
      pure [n]
    Big _ -> do
      rest <- concatMap (findPath2 m seen) neighbors
      pure (n:rest)
    Small _ -> do
      guard
        $ not
        $ or [ and [ any (==2) (M.elems seen), M.lookup n seen == Just 1 ]
             , M.lookup n seen == Just 2
             ]
      rest <- concatMap (findPath2 m seen') neighbors
      pure (n:rest)
    Start -> do
      guard $ isNothing (M.lookup n seen)
      rest <- concatMap (findPath2 m seen') neighbors
      pure (Start:rest)

findPath1 :: AdjList -> Map Node Int -> Node -> [[Node]]
findPath1 m seen n = do
  let
    neighbors = S.toList (m M.! n)
    seen' = M.insertWith (+) n 1 seen
  case n of
    End ->
      pure [n]
    Big _ -> do
      rest <- concatMap (findPath1 m seen) neighbors
      pure (n:rest)
    _ -> do
      guard $ isNothing (M.lookup n seen)
      rest <- concatMap (findPath1 m seen') neighbors
      pure (n:rest)

mkAdjList :: [(Node,Node)] -> Map Node (Set Node)
mkAdjList nodes =
   foldl1 (M.unionWith (<>))
   [ M.singleton x (S.singleton y) <> M.singleton y (S.singleton x)
   | (x,y) <- nodes
   ]

type AdjList = Map Node (Set Node)

data Node = Start | End | Small String | Big String
  deriving (Eq, Ord)

instance Show Node where
  show Start = "start"
  show End = "end"
  show (Small s) = s
  show (Big s) = s

parse :: String -> (Node,Node)
parse xs =
  case parseSized <$> splitOn "-" xs of
    [l,r] -> (l, r)

parseSized :: String -> Node
parseSized x
  | x == "start" = Start
  | x == "end" = End
  | all isLower x = Small x
  | otherwise = Big x
