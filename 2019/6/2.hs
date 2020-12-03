import Prelude hiding (lookup)
import Data.Maybe (maybe)
import Data.Map (Map, singleton , insert, updateLookupWithKey, lookup)
import Data.List.Split (splitOn)

type APA = Maybe (Maybe Integer, Maybe Integer)
type UOM = Map String (APA, String)

emptyUOM :: UOM
emptyUOM = singleton "COM" (Nothing, "")

updateOrbit :: UOM -> String -> (APA -> Integer -> APA) -> UOM
updateOrbit m a e = update m a 0
  where
    update m a u = maybe m' (\(_, a') -> update m' a' (u + 1)) v
      where (v, m') = updateLookupWithKey (\_ (n, a) -> Just (e n u, a)) a m

updateOrbitFromParent :: UOM -> String -> (APA -> Integer -> APA) -> UOM
updateOrbitFromParent m a e = case lookup a m of
  (Just (_, a')) -> updateOrbit m a' e
  _              -> m

fromSAN :: APA -> Integer -> APA
fromSAN (Just (s, Just y))  u = Just (Just $ y + u, s)
fromSAN (Just (s, Nothing)) u = Just (Just u, s)
fromSAN Nothing             u = Just (Just u, Nothing)

fromYOU :: APA -> Integer -> APA
fromYOU (Just (s, Just y))  u = Just (s, Just $ y + u)
fromYOU (Just (s, Nothing)) u = Just (s, Just u)
fromYOU Nothing             u = Just (Nothing, Just u)

insertOrbit :: UOM -> String -> String -> UOM
insertOrbit m b a = insert b (Nothing, a) m

parseUOM :: [String] -> UOM
parseUOM ls = parse ls emptyUOM
  where
    parse []     m = m
    parse (l:ls) m = parse ls (insertOrbit m b a)
      where
        [a, b] = splitOn ")" l

getShortest :: Maybe Integer -> (APA, String) -> Maybe Integer
getShortest (Just t) (Just (Just a, Just b), _) = Just $ if t' < t then t' else t
  where t' = a + b
getShortest Nothing  (Just (Just a, Just b), _) = Just $ a + b
getShortest t        _                          = t

main :: IO ()
main = do
  ls <- lines <$> readFile "input.txt"
  let m = updateOrbitFromParent (updateOrbitFromParent (parseUOM ls) "SAN" fromSAN) "YOU" fromYOU
  print $ foldl getShortest Nothing m