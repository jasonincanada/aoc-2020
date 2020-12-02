module Day01 (part1, part2) where

{-  Advent of Code 2020 - Day 1 - https://adventofcode.com/2020/day/1 -}

import Scanner


{- Types -}

type Input  = [Int]
data Output = Output Int

instance Show Output where
  show (Output prod) = show prod


{- Parsing -}

parseInput :: Scanner Input
parseInput = many int


{- Methods -}

calc1 :: Input -> Output
calc1 entries = Output result
  where
    result = head [ m*n | m <- entries,
                          n <- entries,
                          m+n == 2020 ]


calc2 :: Input -> Output
calc2 entries = Output result
  where
    result = head [ x*y*z | x <- entries,
                            y <- entries,
                            z <- entries,
                            x+y+z == 2020 ]


{- Operations -}

part1 :: String -> String
part1 = show . calc1 . parse
  where
    parse = runLineScanner parseInput


part2 :: String -> String
part2 = show . calc2 . parse
  where
    parse = runLineScanner parseInput


