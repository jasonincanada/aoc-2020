{-# Language MultiWayIf #-}

module Day08 where

{-  Advent of Code 2020 - Day 8 - https://adventofcode.com/2020/day/8 -}

import qualified Data.Map as M
import Control.Monad.State
import Data.Function       ((&))
import Data.Maybe          (mapMaybe)


{- Types -}

data Instr   = Nop Int
             | Acc Int
             | Jmp Int

data Result  = InfiniteLoop
             | Finished Int
               deriving Eq

type Address = Int
type Program = M.Map Address Instr

data Console = Console { path    :: [Address]  -- addresses visited
                       , acc     :: Int        -- accumulator
                       , ip      :: Address    -- next instruction to run
                       , program :: Program
                       }

type Input   = Program
data Output  = Output Int

instance Show Output where
  show (Output count) = show count


{- Parsing -}

parse :: String -> Input
parse = M.fromList . zip [0..] . map (instr . words) . lines
  where
    instr :: [String] -> Instr
    instr ("nop":n:_) = Nop $ toInt n
    instr ("acc":n:_) = Acc $ toInt n
    instr ("jmp":n:_) = Jmp $ toInt n

    -- read can't parse a leading +, so drop them
    toInt :: String -> Int
    toInt = read . dropWhile (=='+')



{- Methods -}

calc1 :: Input -> Output
calc1 program = Output result
  where
    result = execState step start & acc
    start  = Console [] 0 0 program


step :: State Console Result
step = do
  ip    <- gets ip
  path  <- gets path
  size  <- M.size <$> gets program

  if | ip `elem` path -> return InfiniteLoop
     | ip >= size     -> gets acc >>= return . Finished
     | otherwise      ->

         do remember ip

            program <- gets program

            case program M.! ip of
              Nop _ ->  hop 1
              Acc n ->  accumulate n >> hop 1
              Jmp n ->  hop n

            step

  where
    remember :: Address -> State Console ()
    remember ip  = modify $ \c -> c { path = ip : path c }

    hop, accumulate :: Int -> State Console ()
    hop n        = modify $ \c -> c { ip  = ip c  + n }
    accumulate n = modify $ \c -> c { acc = acc c + n }



calc2 :: Input -> Output
calc2 program = Output acc
  where
    Finished acc = dropWhile (==InfiniteLoop) results & head
    results      = run <$> programs
    programs     = mapMaybe alter [0 .. M.size program - 1]
    run          = evalState step . load
    load program = Console [] 0 0 program

    -- if the instruction at the given address is a nop or jmp, return a copy of the
    -- program with that one instruction flipped
    alter :: Address -> Maybe Program
    alter a = case program M.! a of
                Nop n -> Just $ M.insert a (Jmp n) program
                Jmp n -> Just $ M.insert a (Nop n) program
                _     -> Nothing



{- Operations -}

part1 :: String -> String
part1 = show . calc1 . parse
part2 = show . calc2 . parse

