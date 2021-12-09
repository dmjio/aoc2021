module Main where

import           Data.Char          (digitToInt)
import           Data.List          (sortOn, transpose, (\\))
import           Data.List.Split    (splitOn)
import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Maybe         (mapMaybe, catMaybes)
import           Data.Ord           (comparing, Down(Down))

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as I
import           Data.Set           (Set)
import qualified Data.Set           as S

main :: IO ()
main = do
  ls <- fmap parse . lines <$> readFile "input.txt"
  let m = mkMap ls
  part1 m
  part2 m

part1 :: Map (Int, Int) Int -> IO ()
part1 m =
  let
    riskFactor = (+1)
  in
    print $ sum $ fmap (riskFactor . (m M.!)) (getLowPoints m)

part2 :: Map (Int, Int) Int -> IO ()
part2 m =
  print
    $ product
    $ take 3
    $ sortOn Down
    $ getBasin m <$> getLowPoints m

parse :: String -> [Int]
parse = fmap digitToInt

mkMap :: [[Int]] -> Map (Int,Int) Int
mkMap xs =
  let
    x = length xs - 1
    y = length (transpose xs) - 1
    coords = (,) <$> [0..x] <*> [0..y]
  in
    M.fromList (zip coords (concat xs))

getLowPoints :: Map (Int,Int) Int -> [(Int,Int)]
getLowPoints m = M.foldlWithKey go [] m
  where
    go :: [(Int,Int)] -> (Int,Int) -> Int -> [(Int,Int)]
    go acc (x,y) point =
      let
        deltas = [(x,y+1),(x,y-1),(x-1,y),(x+1,y)]
        isLowPoint = all (> point) $ mapMaybe (`M.lookup` m) deltas
      in
        acc ++ [ (x,y) | isLowPoint ]

getBasin :: Map (Int,Int) Int -> (Int,Int) -> Int
getBasin m start = length $ dfs [start] [] []
  where
    dfs [] result _ = result
    dfs (x:xs) acc seen =
      if x `elem` seen
        then
          dfs xs acc seen
        else
          let
            neighbors = (getAdj x m \\ seen) <> xs
          in
            dfs neighbors (1 : acc) (x : seen)

getAdj :: (Int,Int) -> Map (Int,Int) Int -> [(Int,Int)]
getAdj (x,y) m = catMaybes
  [ case M.lookup delta m of
      Nothing -> Nothing
      Just val
        | val > m M.! (x,y) && val /= 9 -> Just delta
        | otherwise -> Nothing
  | delta <- deltas
  ] where
      deltas = [(x,y+1),(x,y-1),(x-1,y),(x+1,y)]
