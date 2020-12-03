import Prelude hiding (lookup)
import Data.Map (Map, fromList, lookup)

checkSlope :: (Map (Integer, Integer) Bool) -> Integer -> Integer -> Integer -> Integer -> Integer
checkSlope a width height dx dy =
  foldl (\n p -> case lookup p a of { (Just True) -> n + 1; _ -> n }) 0 (zip (map (flip rem width) [0,dx..]) [0,dy..(height - 1)])

main = do
  f <- readFile "input.txt"
  let height = toInteger $ length $ lines f
  let width = toInteger $ length $ head $ lines f
  let a = fromList $ (zip [0..] (lines f)) >>= (\(y, r) -> map (\(x, c) -> ((x, y), c == '#')) (zip [0..] r))
  let n1 = checkSlope a width height 1 1
  let n2 = checkSlope a width height 3 1
  let n3 = checkSlope a width height 5 1
  let n4 = checkSlope a width height 7 1
  let n5 = checkSlope a width height 1 2
  print $ n1 * n2 * n3 * n4 * n5