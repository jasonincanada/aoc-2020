module Day06 where

{-  Advent of Code 2020 - Day 6 - https://adventofcode.com/2020/day/6 -}

import Control.Arrow   ((&&&), (>>>))
import Data.Bifunctor  (first)
import Data.List       (group, sort)
import Data.List.Split (splitOn)


{- Types -}

type Group   = [String]
type Input   = [Group]

data Output  = Output Int

instance Show Output where
  show (Output count) = show count


{- Parsing -}

parse :: String -> Input
parse = splitOn [""] . lines


{- Methods -}

calc1 :: Input -> Output
calc1 = Output . sum . map (length . group . sort . concat)

calc2 :: Input -> Output
calc2 = Output . sum . map tally
  where
    -- count the number of times the number of declarants in the group
    -- equals (==) the number of yes answers for a given topic, meaning everyone
    -- in that group answered yes to that topic
    tally :: Group -> Int
    tally = length &&& map length . group . sort . concat   -- (Int        , [Int])
              >>> first (==)                                -- (Int -> Bool, [Int])
              >>> uncurry filter                            -- [Int]
              >>> length

    -- λ> :set -XTypeApplications
    --
    -- (&&&)           :: Arrow a =>     a b c    -> a b c'    -> a b (c, c')
    -- (&&&) @(->)     ::                (b -> c) -> (b -> c') -> (b -> (c, c'))

    -- first           :: Bifunctor p => (a -> b) -> p a c  -> p b c
    -- first @(,)      ::                (a -> b) -> (a, c) -> (b, c)
    -- first @(,) (==) :: Eq a =>                    (a, c) -> (a -> Bool, c)

    -- uncurry :: (a -> b -> c) -> (a, b) -> c
    -- filter  :: (a -> Bool) -> [a] -> [a]


{- Operations -}

part1 :: String -> String
part1 = show . calc1 . parse
part2 = show . calc2 . parse

