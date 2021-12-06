{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.IntMap.Strict as IM
import           Data.List.Split    (splitOn)

main :: IO ()
main = do
  nums <- fmap read . splitOn "," <$> readFile "input.txt"
  print (ageMap nums 80)
  print (ageMap nums 256)

ageMap :: [Int] -> Int -> Int
ageMap xs n =
  let
    initial = IM.fromListWith (+) (zip xs (repeat 1))
  in
    IM.foldl' (+) 0 (times stepDay initial n)
  where
    times :: (a -> a) -> a -> Int -> a
    times _ x 0 = x
    times f !x k = times f (f x) (k - 1)

stepDay :: IM.IntMap Int -> IM.IntMap Int
stepDay m = step (IM.assocs m) mempty
  where
    step [] m = m
    step ((0,v):xs) m
      = step xs
      $ IM.insertWith (+) 6 v
      . IM.insertWith (+) 8 v
      $ m

    step ((k,v):xs) m
      = step xs
      $ IM.insertWith (+) (k - 1) v
      $ m
