{-# LANGUAGE MultilineStrings #-}

import Data.List (elemIndex, find)
import Data.Map (lookup, size)
import qualified Data.Map as Map (empty, insert)
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set, fromList, insert, member)
import qualified Data.Set as Set (empty, insert)
import Prelude hiding (lookup)
import System.IO (readFile)

example = """
  .......S.......
  ...............
  .......^.......
  ...............
  ......^.^......
  ...............
  .....^.^.^.....
  ...............
  ....^.^...^....
  ...............
  ...^.^...^.^...
  ...............
  ..^...^.....^..
  ...............
  .^.^.^.^.^...^.
  ...............
  """

parse input =
  let l = lines input
  in ((fromJust . (elemIndex 'S') . head) l
    , (fromList .
      (mapMaybe (\(p, c) -> case c of '^' -> Just p; _   -> Nothing)) .
      ((=<<) (\(y, l) -> zipWith (\x c -> ((x, y), c)) [0..] l)) .
      (zip [1..]) .
      tail) l
    , length l)

findNextSplitter bottom splitters (x, y) =
  ((find ((flip member) splitters)) . (map (\y -> (x, y)))) [y..bottom]

countTimelines bottom splitters p hitSplitters =
  case findNextSplitter bottom splitters p of
    Just (x, y) -> case lookup (x, y) hitSplitters of
      Just timelines -> (timelines, hitSplitters)
      Nothing -> let
          (timelinesLeft, hitSplittersLeft) = countTimelines bottom splitters (x - 1, y) hitSplitters
          (timelinesRight, hitSplittersRight) = countTimelines bottom splitters (x + 1, y) hitSplittersLeft
        in (timelinesLeft + timelinesRight, Map.insert (x, y) (timelinesLeft + timelinesRight) hitSplittersRight)
    Nothing -> (1, hitSplitters)

main = do
  (start, splitters, bottom) <- parse <$> readFile "input.txt"
  let (timelines, hitSplitters) = countTimelines bottom splitters (start, 0) Map.empty
  putStrLn ("Part 1: " ++ (show (size hitSplitters)))
  putStrLn ("Part 2: " ++ (show timelines))
