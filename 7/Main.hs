module Main where

import Data.List (nub)
import Data.List.Split (splitOn)

test :: [Int]
test = [16,1,2,0,4,2,7,1,2,14]

main :: IO ()
main = do
  nums <- fmap read . splitOn "," <$> readFile "input.txt"
  print $ alignPt1 test
  print $ alignPt2 test

  print $ alignPt1 nums
  print $ alignPt2 nums

alignPt1 :: [Int] -> Int
alignPt1 xs =
  minimum
    [ sum $ zipWith (\x y -> abs (x - y)) xs (repeat x)
    | x <- nub xs
    ]

alignPt2 :: [Int] -> Int
alignPt2 xs =
  minimum
  [ sum $ zipWith series xs (repeat x)
  | x <- [ minimum xs .. maximum xs ]
  ] where
      series x y =
        let
          n = abs (x `subtract` y)
        in
          n * (n + 1) `div` 2
