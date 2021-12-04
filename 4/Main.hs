{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import           Data.List          (find, transpose)
import           Data.List.Split    (splitOn, chunksOf)

main :: IO ()
main = do
  ins : (drop 1 -> boards) <- lines <$> readFile "input.txt"
  let
    nums :: [Int]
    nums = fmap read (splitOn "," ins)

  print $ firstWinner (parseBoards boards) nums
  print $ lastWinner (parseBoards boards) nums

type Board = [(Int,Bool)]

parseBoards :: [String] -> [Board]
parseBoards = fmap (fillBoard . parseBoard) . makeBoards
  where
    parseRow :: String -> [Int]
    parseRow = map read . words

    parseBoard :: [String] -> [[Int]]
    parseBoard = fmap parseRow

    makeBoards :: [String] -> [[String]]
    makeBoards xs = splitOn [""] xs

    fillBoard :: [[Int]] -> Board
    fillBoard (concat -> xs) =
      fmap (,False) xs

checkWinner :: Board -> Bool
checkWinner m = or
  [ or [ and (fmap snd row)
       | row <- chunksOf 5 m
       ]
  , or [ and (fmap snd col)
       | col <- transpose (chunksOf 5 m)
       ]
  ]

markHit :: Int -> Board -> Board
markHit n =
  fmap (\(k,v) -> if n == k then (k,True) else (k,v))

sumUnmarked :: Board -> Int
sumUnmarked
  = sum
  . fmap (\(k,v) -> if v == False then k else 0)

firstWinner :: [Board] -> [Int] -> Int
firstWinner _ [] = error "firstWinner: out of numbers"
firstWinner boards (x:xs) =
  let
    newBoards = fmap (markHit x) boards
  in
    case find checkWinner newBoards of
      Nothing ->
        firstWinner newBoards xs
      Just winner ->
        sumUnmarked winner * x

lastWinner :: [Board] -> [Int] -> Int
lastWinner _ [] = error "lastWinner: out of numbers"
lastWinner boards (x:xs) =
  let
    newBoards = fmap (markHit x) boards
  in
    case find checkWinner newBoards of
      Nothing ->
        lastWinner newBoards xs
      Just winner
        | length newBoards == 1 -> sumUnmarked winner * x
        | otherwise ->
            lastWinner (filter (not . checkWinner) newBoards)
              xs
