{-# LANGUAGE TupleSections #-}
module Main where

import           Data.Char       (intToDigit)
import           Data.List       (sort, permutations)
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import qualified Data.Map        as M

type SigPattern = [String]
type Output     = [String]

main :: IO ()
main = do
  ls <- lines <$> readFile "input.txt"
  part1 ls
  part2 ls

part1 :: [String] -> IO ()
part1 input = print $ length
  [ output
  | output <- snd . parse =<< input
  , length output `elem` [2,3,4,7]
  ]

part2 :: [String] -> IO ()
part2 = print . sum . fmap (solve . parse)
  where
    solve :: (SigPattern, Output) -> Int
    solve (sig, out) = applyMapping (findMapping sig) out

parse :: String -> (SigPattern, Output)
parse xs =
  case splitOn ["|"] (words xs) of
    [ls,rs] -> (ls,rs)

applyMapping :: Map Char Segment -> [String] -> Int
applyMapping m outputs =
  let
    segs :: [[Segment]]
    segs = [ sort segment
           | output <- outputs
           , let segment = (m M.!) <$> output
           ]
  in
    read $ intToDigit . (position M.!) <$> segs

-- | Lookup segments in random position mapping
-- Return True if all exist, a valid mapping has been found
findMapping :: [String] -> Map Char Segment
findMapping keys = head $ filter predicate perms
  where
    perms :: [Map Char Segment]
    perms =
      [ M.fromList (zip k v)
      | v <- permutations [Top ..]
      , let k = ['a' .. 'g']
      ]

    predicate :: Map Char Segment -> Bool
    predicate m =
      let
        positions :: Maybe [Int]
        positions = fmap sort $ sequence
          [ M.lookup (sort segment) position
          | segment <-
              [ fmap (m M.!) key
              | key <- keys
              ]
          ]
      in
        positions == Just [0..9]

data Segment
  = Top
  | Mid
  | Bottom
  | LTop
  | LBottom
  | RTop
  | RBottom
  deriving (Show, Eq, Ord, Enum)

position :: Map [Segment] Int
position =
  M.fromList
  [ (sort (Top : [Bottom ..]), 0)
  , (sort [RTop, RBottom], 1)
  , (sort [Top, RTop, Mid, LBottom, Bottom], 2)
  , (sort [Top, RTop, Mid, RBottom, Bottom], 3)
  , (sort [LTop, RTop, Mid, RBottom], 4)
  , (sort [Top, LTop, Mid, RBottom, Bottom], 5)
  , (sort [Top, LTop, Mid, LBottom, RBottom, Bottom], 6)
  , (sort [Top, RTop, RBottom], 7)
  , (sort [Top .. RBottom], 8)
  , (sort [Top,LTop,RTop,Mid,RBottom,Bottom], 9)
  ]
