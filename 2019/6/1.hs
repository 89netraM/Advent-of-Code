import Prelude hiding (lookup)
import Data.Maybe (maybe)
import Data.Map (Map, singleton , insert, updateLookupWithKey, lookup, foldrWithKey)
import Data.List.Split (splitOn)

type UOM = Map String (Integer, String)

emptyUOM :: UOM
emptyUOM = singleton "COM" (0, "")

updateOrbit :: UOM -> String -> UOM
updateOrbit m a = update m a 0
  where
    update m a u = maybe m' (\(_, a') -> update m' a' (u + 1)) v
      where (v, m') = updateLookupWithKey (\_ (n, a) -> Just (n + u, a)) a m

insertOrbit :: UOM -> String -> String -> UOM
insertOrbit m b a = insert b (0, a) m

parseUOM :: [String] -> UOM
parseUOM ls = parse ls emptyUOM
  where
    parse []     m = m
    parse (l:ls) m = parse ls (insertOrbit m b a)
      where
        [a, b] = splitOn ")" l

totalOrbits :: UOM -> Integer
totalOrbits m = maybe 0 fst (lookup "COM" m)

main = do
  ls <- lines <$> readFile "input.txt"
  let m = parseUOM ls
  print $ totalOrbits $ foldrWithKey (\k _ m -> updateOrbit m k) m m