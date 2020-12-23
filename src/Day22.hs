module Day22 where

{-  Advent of Code 2020 - Day 22 - https://adventofcode.com/2020/day/22 -}

import Data.List.Split (splitOn)


{- Types -}

type Card    = Int
type Input   = ([Card], [Card])

data Output  = Output Int

instance Show Output where
  show (Output result) = show result


{- Parsing -}

parse :: String -> Input
parse = tuple 
          . map (map read . tail)
          . splitOn [""]
          . lines
  where
    tuple (x:y:_) = (x,y)



{- Methods -}

calc1 :: Input -> Output
calc1 input = Output result
  where
    result = play input

    play :: ([Card],[Card]) -> Int

    -- both players still have cards, continue playing
    play (one:ones, two:twos)
      | one > two  = play (ones ++ [one,two], twos             )
      | two > one  = play (ones             , twos ++ [two,one])

    -- one player has run out of cards, calculate the win for the other player
    play (ones, []) = calculate ones
    play ([], twos) = calculate twos

    calculate :: [Card] -> Int
    calculate cards = sum $ zipWith (*) [1..] $ reverse cards


calc2 :: Input -> Output
calc2 input = Output result
  where
    result = 0



{- Operations -}

part1 :: String -> String
part1 = show . calc1 . parse
part2 = show . calc2 . parse

