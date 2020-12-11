{-# Language ViewPatterns #-}

module Day10 where

{-  Advent of Code 2020 - Day 10 - https://adventofcode.com/2020/day/10 -}

import Data.List  (sort)


{- Types -}

type Input   = [Int]
data Output  = Output Int

instance Show Output where
  show (Output count) = show count


{- Parsing -}

parse :: String -> Input
parse = map read . lines


{- Methods -}

calc1 :: Input -> Output
calc1 (sort -> jolts) = Output result
  where
    result  = ones * threes
    highest = last jolts
    
    diffs   = zipWith (-) (jolts ++ [highest+3]) (0 : jolts)
    ones    = length $ filter (==1) diffs
    threes  = length $ filter (==3) diffs


calc2 :: Input -> Output
calc2 input = Output result
  where
    result = 0



{- Operations -}

part1 :: String -> String
part1 = show . calc1 . parse
part2 = show . calc2 . parse

