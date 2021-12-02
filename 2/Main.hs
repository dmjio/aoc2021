{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.List

main :: IO ()
main = do
  nums <- fmap parse . lines <$> readFile "input.txt"
  print (part1 nums)
  print (part2 nums)

part1 :: [(Dir, Int)] -> Int
part1 = uncurry (*) . foldl' go (0,0)
  where
    go :: (Int,Int) -> (Dir,Int) -> (Int,Int)
    go (x,y) (Up,d) = (x, y-d)
    go (x,y) (Down,d) = (x, y+d)
    go (x,y) (Forward,d) = (x+d, y)

part2 :: [(Dir, Int)] -> Int
part2 xs = let (x,y,_) = foldl' go (0,0,0) xs in x * y
  where
    go :: (Int,Int,Int) -> (Dir,Int) -> (Int,Int,Int)
    go (x,y,aim) (Up,d) = (x, y, aim-d)
    go (x,y,aim) (Down,d) = (x, y, aim+d)
    go (x,y,aim) (Forward,d) = (x+d, y + (aim * d), aim)

data Dir = Up | Down | Forward
  deriving (Show)

parse :: String -> (Dir, Int)
parse (words -> ["forward", read -> n]) = (Forward, n)
parse (words -> ["up", read -> n]) = (Up, n)
parse (words -> ["down", read -> n]) = (Down, n)
parse _ = error "oops"
