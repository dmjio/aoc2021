{-# LANGUAGE ViewPatterns #-}
module Main where

import           Control.Monad   (forM_)
import           Data.Char       (intToDigit)
import           Data.List.Split (splitOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  xs <- lines <$> readFile "input.txt"
  let coords = fmap parse xs
  print $ overlapCount (populate Part1 coords)
  print $ overlapCount (populate Part2 coords)

parse :: String -> Coord
parse inp =
  case splitOn "->" inp of
    [ splitOn "," -> [read -> x, read -> y]
      , splitOn "," -> [ read -> w ,read -> z]
      ] -> (x, y, w, z)

type Coord  = (Int,Int,Int,Int)
type Coords = [Coord]
type Lines  = [(Int,Int)]
type Grid   = Map (Int,Int) Int

data Part
  = Part1
  | Part2
  deriving (Eq)

coordsToLines :: Part -> Coords -> Lines
coordsToLines part = concatMap go
  where
    go :: Coord -> Lines
    go coord@(x1,y1,x2,y2)
      | x1 == x2 && y1 < y2 = zip (repeat x1) [y1..y2]
      | x1 == x2 && y2 < y1 = zip (repeat x1) [y2..y1]
      | y1 == y2 && x1 < x2 = zip [x1..x2] (repeat y1)
      | y1 == y2 && x2 < x1 = zip [x2..x1] (repeat y1)
      | otherwise =
          case part of
            Part1 -> []
            Part2 -> diags coord

    diags :: Coord -> Lines
    diags (x1,y1,x2,y2)
      | x1 < x2 && y1 < y2 = zip [x1 .. x2] [y1 .. y2]
      | x2 < x1 && y1 < y2 = zip (reverse [x2 .. x1]) [y1 .. y2]
      | x1 < x2 && y2 < y1 = zip [x1 .. x2] (reverse [y2 .. y1])
      | x2 < x1 && y2 < y1 = zip (reverse [x2 .. x1]) (reverse [y2 .. y1])
      | otherwise = error "oh no"

populate :: Part -> Coords -> Grid
populate part coords =
  M.fromListWith (+)
    (zip (coordsToLines part coords) (repeat 1))

overlapCount :: Grid -> Int
overlapCount = M.size . M.filter (>1)

showGrid :: Int -> Grid -> IO ()
showGrid n m = do
  forM_ [0..n] $ \y -> do
    forM_ [0..n] $ \x ->
      case M.lookup (x,y) m of
        Nothing -> putChar '.'
        Just num -> putChar (intToDigit num)
    putChar '\n'
