{-# LANGUAGE ViewPatterns #-}
module Main where

import           Control.Arrow   ((***))
import           Control.Monad   (forM_)
import           Data.List       (foldl')
import           Data.List.Split (splitOn)
import           Data.Set        (Set)
import qualified Data.Set        as S

main :: IO ()
main = do
  input <- parse . lines <$> readFile "input.txt"
  part1 input
  part2 input

part1 :: Input -> IO ()
part1 (nums, actions)
  = print
  $ S.size
  $ coords
  $ foldPaper nums (take 1 actions)

part2 :: Input -> IO ()
part2 (nums, actions)
  = showPaper
  $ foldPaper nums actions

data Paper
  = Paper
  { coords :: PaperCoords
  , x :: Int
  , y :: Int
  } deriving Eq

data Axis
  = X
  | Y
  deriving (Eq)

type PaperCoords = Set (Int,Int)
type Action      = (Axis,Int)
type Coord       = (Int,Int)
type Input       = ([Coord],[Action])

foldPaper :: [(Int,Int)] -> [(Axis,Int)] -> Paper
foldPaper coords = foldl' step (mkPaper coords)

step :: Paper -> (Axis,Int) -> Paper
step (Paper set maxX maxY) (axis,val) =
  let
    set' = S.map alterKey set

    alterKey (x,y)
      | X <- axis, x > val = (val - (x - val), y)
      | Y <- axis, y > val = (x, val - (y - val))
      | otherwise = (x,y)

    (maxX',maxY')
      | X <- axis = ((maxX - val), maxY)
      | Y <- axis = (maxX, (maxY - val))
  in
    Paper set' maxX' maxY'

parse :: [String] -> Input
parse xs =
  case splitOn [""] xs of
    [ls,rs] ->
      (fmap parseCoord *** fmap parseAction)
        (ls,rs)
  where
    parseAction :: String -> Action
    parseAction zs =
      case splitOn "=" $ last (words zs) of
        [parseVar -> var, read -> num] -> (var,num)

    parseCoord :: String -> Coord
    parseCoord zs =
      case splitOn "," zs of
        [read -> x, read -> y] -> (x,y)

    parseVar "x" = X
    parseVar "y" = Y
    parseVar _ = error "oops"

mkPaper :: [(Int,Int)] -> Paper
mkPaper xs@(unzip -> (maximum -> x, maximum -> y)) =
  Paper (S.fromList xs) x y

showPaper :: Paper -> IO ()
showPaper (Paper s x y) = do
  forM_ [0..y - 1] $ \y' -> do
    forM_ [0..x - 1] $ \x' ->
      if S.member (x',y') s
        then putChar '#'
        else putChar '.'
    putStrLn ""
