import Prelude hiding (lookup)
import Data.List.Split (splitOn)
import Data.Map (Map, empty, insert, lookup, elems)
import Data.Maybe (fromJust)

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

getParams :: Program -> Integer -> [Bool] -> [Integer]
getParams p offset [] = []
getParams p offset (False:bs) = fromJust (lookup (fromJust (lookup (offset + 1) p)) p) : getParams p (offset + 1) bs
getParams p offset (True:bs) = fromJust (lookup (offset + 1) p) : getParams p (offset + 1) bs

executeInstruction :: Program -> Integer -> [Integer] -> IO Program
executeInstruction p 1 [i1, i2, o] = return $ insert o (i1 + i2) p
executeInstruction p 2 [i1, i2, o] = return $ insert o (i1 * i2) p
executeInstruction p 3 [o]         = do
  int <- read <$> getLine
  return $ insert o int p
executeInstruction p 4 [i]         = do
  print i
  return p

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
          let n' = n + toInteger (length s) + 1
          let params = getParams p n s
          p' <- executeInstruction p op params
          exe p' n'

main = do
  p <- readProgram "input.txt"
  p' <- executeProgram p
  print (elems p')