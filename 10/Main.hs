module Main where

import Data.List (group, sort, foldl')

main :: IO ()
main = do
  ls <- lines <$> readFile "input.txt"
  print (part1 ls)
  print (part2 ls)

data Process
  = Corrupted Char Char
  | Incomplete String
  | Good
  deriving Show

part1 :: [String] -> Int
part1 xs
  = sum
  $ fmap sum
  $ (fmap . fmap) score
  $ group
  $ sort
  [ e
  | Corrupted e _ <- fmap runner xs
  ] where
      score ')' = 3
      score ']' = 57
      score '}' = 1197
      score '>' = 25137
      score _ = 0

part2 :: [String] -> Int
part2 xs
  = middleScore
  $ sort
  [ calcScore i
  | Incomplete i <- fmap runner xs
  ] where
      score :: Char -> Int
      score ')' = 1
      score ']' = 2
      score '}' = 3
      score '>' = 4
      score _ = 0

      calcScore :: String -> Int
      calcScore = foldl' go 0
        where
          go :: Int -> Char -> Int
          go acc c = score c + (5 * acc)

      middleScore :: [a] -> a
      middleScore [] = error "no middle"
      middleScore [x] = x
      middleScore xs = middleScore (init (tail xs))

runner :: String -> Process
runner xs = go xs []
  where
    go :: String -> String -> Process
    go ('{':xs) stack =
      go xs ('}':stack)
    go ('}':xs) ('}':stack) =
      go xs stack
    go ('}':xs) (c:stack) =
      Corrupted '}' c

    go ('[':xs) stack =
      go xs (']':stack)
    go (']':xs) (']':stack) =
      go xs stack
    go (']':xs) (c:stack) =
      Corrupted ']' c

    go ('(':xs) stack =
      go xs (')':stack)
    go (')':xs) (')':stack) =
      go xs stack
    go (')':xs) (c:stack) =
      Corrupted ')' c

    go ('<':xs) stack =
      go xs ('>':stack)
    go ('>':xs) ('>':stack) =
      go xs stack
    go ('>':xs) (c:stack) =
      Corrupted '>' c

    go [] [] = Good
    go [] xs = Incomplete xs
