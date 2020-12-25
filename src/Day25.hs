module Day25 where

{-  Advent of Code 2020 - Day 25 - https://adventofcode.com/2020/day/25 -}

import Data.Function ((&))


{- Types -}

type Subject = Int
type PubKey  = Int

type Input   = (PubKey,PubKey)
data Output  = Output Int

instance Show Output where
  show (Output enc) = show enc


{- Parsing -}

parse :: String -> Input
parse = tuple . map read . lines
  where
    tuple (x:y:_) = (x,y)



{- Methods -}

-- given in the problem description
initial = 7
modulus = 20201227


-- arbitrarily choose the key, we could have also run both calculations in parallel
-- and taken the first loop size found
calc1 :: Input -> Output
calc1 (key,door) = Output result
  where
    result   = transforms door !! loopsize
                 & snd

    loopsize = transforms initial
                 & dropWhile ((key /=) . snd)
                 & head
                 & fst

    transforms :: Subject -> [(Int,PubKey)]
    transforms sub = zip [0..] (iterate step 1)
      where
        step :: PubKey -> PubKey
        step n = n * sub `mod` modulus


-- n/a for day 25
calc2 :: Input -> Output
calc2 input = Output result
  where
    result = 0



{- Operations -}

part1 :: String -> String
part1 = show . calc1 . parse
part2 = show . calc2 . parse

