module Day09 where

{-  Advent of Code 2020 - Day 9 - https://adventofcode.com/2020/day/9 -}

import Data.Function ((&))
import Data.Maybe    (mapMaybe)
import Data.List     (tails)


{- Types -}

type Input   = [Int]
data Output  = Output Int

instance Show Output where
  show (Output count) = show count


{- Parsing -}

parse :: String -> Input
parse = map read . lines


{- Methods -}

-- from the problem description (set to 5 to make the test pass)
preamble = 25

calc1 :: Input -> Output
calc1 numbers = Output result
  where
    result = go (reverse $ take preamble numbers) (drop preamble numbers)

    go :: [Int] -> [Int] -> Int
    go stack (n:rest) = if nosum
                        then n
                        else go (n : init stack) rest
      where
        nosum = null [ n | x <- stack,
                           y <- stack,
                           x /= y,
                           x + y == n ]


-- the answer to part 1 for my input (set to 127 to make the test pass)
target = 258585477

calc2 :: Input -> Output
calc2 numbers = Output result
  where
    result = mapMaybe go (tails numbers) & head & uncurry (+)

    go :: [Int] -> Maybe (Int, Int)
    go nums = f 0 nums []
      where
        f _   []       _     = Nothing
        f acc (n:rest) stack = if acc + n == target
                               then Just (minimum (n:stack), maximum (n:stack))
                               else f (acc+n) rest (n:stack)



{- Operations -}

part1 :: String -> String
part1 = show . calc1 . parse
part2 = show . calc2 . parse

