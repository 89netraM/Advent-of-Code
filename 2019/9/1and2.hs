{-# LANGUAGE LambdaCase #-}

import Prelude hiding (lookup, (!))
import Data.List.Split (splitOn)
import Data.Map (Map, empty, fromList, insert, lookup, (!))
import Data.Maybe (fromMaybe)

type Program = Map Integer Integer
data State = State
  { prog :: Program
  , base :: Integer
  }
failureState :: State
failureState = State (fromList [(0, 99)]) 0

data PCChange = Move
              | Goto Integer
move :: (Monad a) => b -> a (b, PCChange)
move e = return (e, Move)
goto :: (Monad a) => Integer -> b -> a (b, PCChange)
goto a e = return (e, Goto a)

data ParamMode = Position
               | Immediate
               | Relative
  deriving (Show)

readProgram :: FilePath -> IO Program
readProgram file = do
  content <- readFile file
  return $ foldl (\p (i, e) -> insert i e p) empty $ zip [0..] $ map read $ splitOn "," content

getParamMode :: Integer -> Integer -> ParamMode
getParamMode ins pos = case (ins `div` pos) `rem` 10 of
  0 -> Position
  1 -> Immediate
  2 -> Relative

getOp :: Integer -> Integer
getOp = flip rem 100

getParamCount :: Integer -> Integer
getParamCount = \case
  1 -> 3
  2 -> 3
  3 -> 1
  4 -> 1
  5 -> 2
  6 -> 2
  7 -> 3
  8 -> 3
  9 -> 1
  _ -> 0
getParamConfigs :: Integer -> Integer -> [ParamMode]
getParamConfigs c ins = [getParamMode ins ((10 ^ m) * 10) | m <- [1..c]]

getParams :: State -> Integer -> [ParamMode] -> [Integer]
getParams _              _      []     = []
getParams st@(State p b) offset (m:ms) = (
    case m of
      Position  -> find p $ offset + 1
      Immediate -> offset + 1
      Relative  -> (b+) $ find p $ offset + 1
  ) : getParams st (offset + 1) ms

find :: Program -> Integer -> Integer
find p i = fromMaybe 0 (lookup i p)

executeInstruction :: State -> Integer -> [Integer] -> IO (State, PCChange)
executeInstruction st@(State p _) 1 [i1, i2, o] = move st{prog = insert o (find p i1 + find p i2) p}
executeInstruction st@(State p _) 2 [i1, i2, o] = move st{prog = insert o (find p i1 * find p i2) p}
executeInstruction st@(State p _) 3 [o] = do
  putStr "> "
  int <- read <$> getLine
  move st{prog = insert o int p}
executeInstruction st@(State p _) 4 [i] = do
  putStrLn $ "< " ++ show (find p i)
  move st
executeInstruction st@(State p _) 5 [i1, i2] = (if find p i1 /= 0 then goto $ find p i2 else move) st
executeInstruction st@(State p _) 6 [i1, i2] = (if find p i1 == 0 then goto $ find p i2 else move) st
executeInstruction st@(State p _) 7 [i1, i2, o] = move st{prog = insert o (if find p i1 < find p i2 then 1 else 0) p}
executeInstruction st@(State p _) 8 [i1, i2, o] = move st{prog = insert o (if find p i1 == find p i2 then 1 else 0) p}
executeInstruction st@(State p b) 9 [i] = move st{base = b + find p i}
executeInstruction _ op _ = do
  putStrLn $ "Error: Unknown op " ++ show op
  goto 0 failureState

executeProgram :: State -> IO State
executeProgram st = exe st 0
  where
    exe st pc = do
      let ins = prog st ! pc
      let op = getOp ins
      if op == 99
        then return st
        else do
          let paramCount = getParamCount op
          let paramConfigs = getParamConfigs paramCount ins
          let params = getParams st pc paramConfigs
          (st', nUpdate) <- executeInstruction st op params
          case nUpdate of
            Move     -> exe st' (pc + 1 + paramCount)
            Goto pc' -> exe st' pc'

main :: IO ()
main = do
  p <- readProgram "input.txt"
  executeProgram $ State p 0
  return ()