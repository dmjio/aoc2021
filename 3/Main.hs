module Main where

import Data.Bool ( bool )
import Control.Arrow ( Arrow((&&&)) )
import Data.List ( foldl', transpose )
import Data.Char ( digitToInt )

main :: IO ()
main = do
  part1
  part2

part1 :: IO ()
part1 = do
  nums <- transpose . lines <$> readFile "input.txt"
  let
    solve =
      [ bool '0' '1' (length zeros > length ones)
      | (zeros, ones) <- split <$> nums
      ]
    gamma = solve
    epsilon = flipBits solve
  print (toDec epsilon * toDec gamma)

flipBits :: String -> String
flipBits = fmap flipBit
  where
    flipBit '1' = '0'
    flipBit '0' = '1'
    flipBit _ = error "oops"

part2 :: IO ()
part2 = do
  nums <- lines <$> readFile "input.txt"
  let
    ox = toDec (solve '0' nums)
    co = toDec (solve '1' nums)
  print (ox * co)

solve :: Char -> [String] -> String
solve c = go 0
  where
    go :: Int -> [String] -> String
    go _ [x] = x
    go n xs =
      let
        (zeroes, ones) = split (fmap (!! n) xs)
      in
        go (n+1) $
          if length zeroes > length ones
          then filter ((==c) . (!!n)) xs
          else filter ((/=c) . (!!n)) xs

split :: String -> (String, String)
split = filter (=='0') &&& filter (=='1')

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0
