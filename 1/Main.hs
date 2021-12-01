module Main where

main :: IO ()
main = do
  nums <- fmap read . lines <$> readFile "input.txt"

  -- part 1
  print
    $ countIncreasing nums

  -- part 2
  print
    $ countIncreasing
    $ fmap sum
    $ sliding 3 nums

countIncreasing
  :: Ord a
  => [a]
  -> Int
countIncreasing xs
  = length
  $ filter (uncurry (<))
  $ zip xs (tail xs)

sliding
  :: Int
  -> [Int]
  -> [[Int]]
sliding _ [] = []
sliding size xs
  | length xs < size = []
  | otherwise =
      take size xs :
        sliding size (drop 1 xs)
