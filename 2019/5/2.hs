import Prelude hiding (lookup)
import Data.List.Split (splitOn)
import Data.Map (Map, empty, insert, lookup, elems)
import Data.Maybe (fromJust)
import Data.Either (Either)
import Debug.Trace (traceShowId)

type Program = Map Integer Integer

readProgram :: FilePath -> IO Program
readProgram file = do
  content <- readFile file
  return $ foldl (\p (i, e) -> insert i e p) empty $ zip [0..] $ map read $ splitOn "," content

getParamMode :: Integer -> Bool
getParamMode ins = rem ins 10 == 1

getOp :: Integer -> Integer
getOp = flip rem 100

getParamSize :: Integer -> Integer -> [Bool]
getParamSize 1 ins = map (getParamMode . div ins) [100, 1000] ++ [True]
getParamSize 2 ins = map (getParamMode . div ins) [100, 1000] ++ [True]
getParamSize 3 ins = [True]
getParamSize 4 ins = [getParamMode (ins `div` 100)]
getParamSize 5 ins = map (getParamMode . div ins) [100, 1000]
getParamSize 6 ins = map (getParamMode . div ins) [100, 1000]
getParamSize 7 ins = map (getParamMode . div ins) [100, 1000] ++ [True]
getParamSize 8 ins = map (getParamMode . div ins) [100, 1000] ++ [True]

getParams :: Program -> Integer -> [Bool] -> [Integer]
getParams p offset [] = []
getParams p offset (False:bs) = fromJust (lookup (fromJust (lookup (offset + 1) p)) p) : getParams p (offset + 1) bs
getParams p offset (True:bs) = fromJust (lookup (offset + 1) p) : getParams p (offset + 1) bs

executeInstruction :: Program -> Integer -> [Integer] -> IO (Program, Either Integer Integer)
executeInstruction p 1 [i1, i2, o] = return (insert o (i1 + i2) p, Left 4)
executeInstruction p 2 [i1, i2, o] = return (insert o (i1 * i2) p, Left 4)
executeInstruction p 3 [o]         = do
  putStr "> "
  int <- read <$> getLine
  return (insert o int p, Left 2)
executeInstruction p 4 [i]         = do
  putStrLn $ "< " ++ show i
  return (p, Left 2)
executeInstruction p 5 [i1, i2] = return (
    p,
    if i1 /= 0
      then Right i2
      else Left 3
  )
executeInstruction p 6 [i1, i2] = return (
    p,
    if i1 == 0
      then Right i2
      else Left 3
  )
executeInstruction p 7 [i1, i2, o] = return (
    insert o (if i1 < i2 then 1 else 0) p,
    Left 4
  )
executeInstruction p 8 [i1, i2, o] = return (
    insert o (if i1 == i2 then 1 else 0) p,
    Left 4
  )
executeInstruction p op params = do
  putStrLn $ "Error: " ++ (show op)
  putStrLn $ "\t" ++ (show params)
  return (insert 0 99 empty, Right 0)

executeProgram :: Program -> IO Program
executeProgram p = exe p 0
  where
    exe p n = do
      let ins = fromJust $ lookup n p
      let op = getOp ins
      if op == 99
        then return p
        else do
          let s = getParamSize op ins
          let params = getParams p n s
          (p', nUpdate) <- executeInstruction p op params
          case nUpdate of
            (Left nChange) -> exe p' (n + nChange)
            (Right n') -> exe p' n'

main = do
  p <- readProgram "input.txt"
  p' <- executeProgram p
  print (elems p')