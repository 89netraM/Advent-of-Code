import Prelude hiding (lookup)
import Data.Map (Map, fromList, lookup)

main = do
  f <- readFile "input.txt"
  let height = length $ lines f
  let width = length $ head $ lines f
  let a = fromList $ (zip [0..] (lines f)) >>= (\(y, r) -> map (\(x, c) -> ((x, y), c == '#')) (zip [0..] r))
  let n = foldl (\n p -> case lookup p a of { (Just True) -> n + 1; _ -> n }) 0 (zip (map (flip rem width) [0,3..]) [0..(height - 1)])
  print n