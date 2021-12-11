module Main where

import           Control.Monad   (forM_)
import           Data.Char       (digitToInt)
import           Data.List       ((\\))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (isJust)
import           Data.Set        (Set)
import qualified Data.Set        as S

main :: IO ()
main = do
  ls <- lines <$> readFile "input.txt"
  let m = parseGrid ls
  print $ part1 m
  print $ part2 m

part1 :: OctoGrid -> Int
part1 m = fst $ iterate step (0,m) !! 100

part2 :: OctoGrid -> Int
part2 = go 0
  where
    go x m =
      let
        (s,g) = step (x,m)
      in
        if all (==0) (M.elems m)
          then x
          else go (x+1) g

type OctoGrid = Map (Int,Int) Int

parseGrid :: [String] -> OctoGrid
parseGrid xs = M.fromList (zip coords energy)
  where
    x = length xs - 1
    coords = (,) <$> [0..x] <*> [0..x]
    energy = fmap digitToInt (concat xs)

step :: (Int, OctoGrid) -> (Int, OctoGrid)
step = step1
  where
    step1 = step2 mempty . (fmap . fmap) (+1)

    step2 :: Set (Int,Int) -> (Int, OctoGrid) -> (Int, OctoGrid)
    step2 seen (flashCount, grid) =
      let
        flashes = M.keys (M.filter (>9) grid) \\ S.toList seen
        adjacents = M.fromListWith (+)
          [ (k,v)
          | (k,v) <- zip (getAdjacent =<< flashes) (repeat 1)
          , isJust (M.lookup k grid)
          ]
      in
        if null flashes
          then (flashCount, set 0 grid seen)
          else
            step2 (seen <> S.fromList flashes)
              ( flashCount + length flashes
              , M.unionWith (+) adjacents grid
              )
      where
        set k = foldr (flip M.insert k)

showGrid :: Int -> OctoGrid -> IO ()
showGrid n m = do
  forM_ [ 0 .. n ] $ \y' -> do
    forM_ [ 0 .. n ] $ \x' -> do
      putChar $ head $ show (m M.! (y',x'))
    putStrLn ""

getAdjacent :: (Int,Int) -> [(Int,Int)]
getAdjacent (x,y) =
  [ (x,y-1)
  , (x,y+1)
  , (x-1,y)
  , (x-1,y-1)
  , (x-1,y+1)
  , (x+1,y)
  , (x+1,y-1)
  , (x+1,y+1)
  ]
