{-# LANGUAGE LambdaCase #-}

import Prelude hiding (lookup, (!))
import GHC.Exts (sortWith)
import Data.List (groupBy)
import Data.List.Split (splitOn)
import Data.Map (Map, empty, fromList, insert, lookup, (!), keys, toList)
import Data.Maybe (fromMaybe)
import Control.Monad.Free (liftF, Free(..))
import qualified Control.Monad.Trans.State as SM

type Program = Map Integer Integer
data State = State
  { prog :: Program
  , base :: Integer
  }

data InterpreterF a
  = Put Integer a
  | Get (Integer -> a)
instance Functor InterpreterF where
  fmap f (Put i  x) = Put i (f x)
  fmap f (Get    x) = Get (f . x)
type Interpreter = Free InterpreterF

get :: Interpreter Integer
get = liftF $ Get id

put :: Integer -> Interpreter ()
put i = liftF $ Put i ()

interpretWith :: Monad m => m Integer -> (Integer -> m ()) -> Interpreter a -> m a
interpretWith _ _ (Pure x) = return x
interpretWith g p (Free (Put i x)) = p i >> interpretWith g p x
interpretWith g p (Free (Get x)) = g >>= interpretWith g p . x

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

executeInstruction :: State -> Integer -> [Integer] -> Interpreter (State, PCChange)
executeInstruction st@(State p _) 1 [i1, i2, o] = move st{prog = insert o (find p i1 + find p i2) p}
executeInstruction st@(State p _) 2 [i1, i2, o] = move st{prog = insert o (find p i1 * find p i2) p}
executeInstruction st@(State p _) 3 [o] = do
  int <- get
  move st{prog = insert o int p}
executeInstruction st@(State p _) 4 [i] = do
  put (find p i)
  move st
executeInstruction st@(State p _) 5 [i1, i2] = (if find p i1 /= 0 then goto $ find p i2 else move) st
executeInstruction st@(State p _) 6 [i1, i2] = (if find p i1 == 0 then goto $ find p i2 else move) st
executeInstruction st@(State p _) 7 [i1, i2, o] = move st{prog = insert o (if find p i1 < find p i2 then 1 else 0) p}
executeInstruction st@(State p _) 8 [i1, i2, o] = move st{prog = insert o (if find p i1 == find p i2 then 1 else 0) p}
executeInstruction st@(State p b) 9 [i] = move st{base = b + find p i}
executeInstruction _ op _ = error $ "Unknown op code: " ++ show op

executeProgram :: State -> Interpreter State
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

data Action
  = Paint
  | Turn
data RobotS = RobotS
  { pos :: (Integer, Integer)
  , dir :: (Integer, Integer)
  , action :: Action
  , panels :: Map (Integer, Integer) Integer
  }
initRobotS :: RobotS
initRobotS = RobotS (0, 0) (0, -1) Paint (fromList [((0, 0), 1)])
interpretWithRobot :: Interpreter a -> Map (Integer, Integer) Integer
interpretWithRobot i = panels m
  where
    (_, m) = SM.runState (interpretWith get' put' i) initRobotS
    get' = do
      s <- SM.get
      let p = pos s
          m = panels s
      return $ fromMaybe 0 $ lookup p m
    put' i = do
      s <- SM.get
      case action s of
        Paint -> do
          let p = pos s
              m = panels s
          SM.put s{ panels = insert p i m, action = Turn }
        Turn -> do
          let (x, y) = pos s
          let newDir@(dx, dy) = case (dir s, i) of
                (( 0, -1), 0) -> (-1,  0)
                ((-1,  0), 0) -> ( 0,  1)
                (( 0,  1), 0) -> ( 1,  0)
                (( 1,  0), 0) -> ( 0, -1)
                (( 0, -1), 1) -> ( 1,  0)
                ((-1,  0), 1) -> ( 0, -1)
                (( 0,  1), 1) -> (-1,  0)
                (( 1,  0), 1) -> ( 0,  1)
                (p,        d) -> error $ "No match for position " ++ show p ++ " and dir " ++ show d
          SM.put s{ pos = (x + dx, y + dy), dir = newDir, action = Paint }

main :: IO ()
main = do
    p <- readProgram "input.txt"
    let m = interpretWithRobot (executeProgram (State p 0))
    let (minX, minY) = foldl1 (\(mx, my) (x, y) -> (min mx x, min my y)) $ keys m
    let m' = unlines $
             map (
               foldr withPadding "" .
               drop 1 .
               reverse .
               foldl foldSep [(minX - 1, 0, '.')] .
               map (\((x, _), c) -> (x, c))
             ) $
             groupBy (\((_, ay), _) ((_, by), _) -> ay == by) $
             sortWith (snd . fst) $
             sortWith (fst . fst) $
             map (\((x, y), v) -> ((x - minX, y - minY), if v == 1 then '█' else '░')) $
             toList m
    putStr m'
  where
    foldSep :: [(Integer, Integer, Char)] -> (Integer, Char) -> [(Integer, Integer, Char)]
    foldSep []                  (x, c) = [(x, 0, c)]
    foldSep (a@(ax, _, _):as) (x, c) = (x, x - ax - 1, c) : a : as
    withPadding :: (Integer, Integer, Char) -> String -> String
    withPadding (_, p, c) s = replicate (fromInteger p) ' ' ++ c : s