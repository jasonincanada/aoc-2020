module Day02 (calc1, calc2, part1, part2, parse) where

{-  Advent of Code 2020 - Day 2 - https://adventofcode.com/2020/day/2 -}

import Data.List.Split (splitOn)


{- Types -}

-- Part 1: min count, max count, character
-- Part 2: first index, second index, character
type Policy   = (Int, Int, Char)  
type Password = String

type Input    = [(Policy, Password)]
data Output   = Output Int

instance Show Output where
  show (Output count) = show count


{- Parsing -}

parse :: String -> Input
parse = map parseLine . lines

parseLine :: String -> (Policy, Password)
parseLine line = (policy, password)
  where
    password = ws !! 2
    policy   = (atLeast, atMost, letter)
    atLeast  = read $ splitOn "-" (ws !! 0) !! 0
    atMost   = read $ splitOn "-" (ws !! 0) !! 1
    letter   = head (ws !! 1)
    ws       = words line


{- Methods -}

calc1 :: Input -> Output
calc1 entries = Output count
  where
    count = length $ filter valid entries
    
    valid :: (Policy,Password) -> Bool
    valid ((from, to, char), password) = from <= countLetter &&
                                           to >= countLetter
      where
        countLetter = length $ filter (==char) password


calc2 :: Input -> Output
calc2 entries = Output count
  where
    count = length $ filter valid entries
    
    valid :: (Policy,Password) -> Bool
    valid ((from, to, char), password) = (password !! (from-1) == char) `xor`
                                         (password !! (to  -1) == char)
    
    xor :: Bool -> Bool -> Bool
    xor True False = True
    xor False True = True
    xor _     _    = False


{- Operations -}

part1 :: String -> String
part1 = show . calc1 . parse
part2 = show . calc2 . parse

