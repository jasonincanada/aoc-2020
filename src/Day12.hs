module Day12 where

{-  Advent of Code 2020 - Day 12 - https://adventofcode.com/2020/day/12 -}

import Control.Arrow  ((&&&))
import Data.Bifunctor (bimap, first, second)
import Data.Function  ((&))


{- Types -}

type Pos     = (Int,Int)
type Dir     = (Int,Int)
type Ship    = (Pos,Dir)

type Ins     = Char
type Dist    = Int

type Input   = [(Ins,Dist)]
data Output  = Output Int

instance Show Output where
  show (Output count) = show count


{- Parsing -}

parse :: String -> Input
parse = map (head &&& read . tail) . lines


{- Methods -}

calc1 :: Input -> Output
calc1 input = Output result
  where
    result = manhattan pos
    pos    = foldl (flip $ uncurry go) ship input & fst
    ship   = ((0,0),(1,0))

    go :: Ins -> Dist -> Ship -> Ship
    go 'N' n   = first  $ second (+n)
    go 'S' n   = first  $ second $ subtract n
    go 'E' n   = first  $ first (+n)
    go 'W' n   = first  $ first $ subtract n

    go 'L' 90  = second $ left
    go 'L' 180 = second $ left . left
    go 'L' 270 = second $ left . left . left

    go 'R' 90  = second $ right
    go 'R' 180 = second $ right . right
    go 'R' 270 = second $ right . right . right

    go 'F' n   = (!!n) . iterate add

    add :: Ship -> Ship
    add ((x,y),(dx,dy)) = ((x+dx, y+dy),(dx,dy))

    manhattan :: Pos -> Int
    manhattan = uncurry (+) . bimap abs abs

    left :: Dir -> Dir
    left ( 1, 0) = ( 0, 1)
    left ( 0, 1) = (-1, 0)
    left (-1, 0) = ( 0,-1)
    left ( 0,-1) = ( 1, 0)

    right :: Dir -> Dir
    right ( 1, 0) = ( 0,-1)
    right ( 0,-1) = (-1, 0)
    right (-1, 0) = ( 0, 1)
    right ( 0, 1) = ( 1, 0)


calc2 :: Input -> Output
calc2 input = Output result
  where
    result = 0



{- Operations -}

part1 :: String -> String
part1 = show . calc1 . parse
part2 = show . calc2 . parse

