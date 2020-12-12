{-# Language TupleSections #-}

module Day11 where

{-  Advent of Code 2020 - Day 11 - https://adventofcode.com/2020/day/11 -}

import qualified Data.Map.Strict as M
import Data.Bifunctor (first)
import Data.Maybe     (mapMaybe)


{- Types -}

-- '#' occupied seat
-- 'L' empty seat
-- '.' floor
type Kind    = Char
type Pos     = (Int,Int)

type SeatMap = M.Map Pos Kind

type Input   = SeatMap
data Output  = Output Int

instance Show Output where
  show (Output count) = show count


{- Parsing -}

parse :: String -> Input
parse = M.fromList
          . concat
          . zipWith (curry push) [0..]
          . map (zip [0..])
          . lines
  where
    push (n,list) = map (first (n,)) list



{- Methods -}

calc1 :: Input -> Output
calc1 seatmap = Output result
  where
    result     = go seatmap
    offsets    = [ (-1,-1), (-1,0), (-1,1),
                   ( 0,-1),         ( 0,1),
                   ( 1,-1), ( 1,0), ( 1,1) ]

    go :: SeatMap -> Int
    go seatmap = let seatmap' = nextmap seatmap
                 in  if seatmap == seatmap'
                     then M.size $ M.filter (=='#') seatmap
                     else go seatmap'

    neighbours :: Pos -> [Pos]
    neighbours pos = map (add pos) offsets
      where
        add (x,y) (dx,dy) = (x+dx,y+dy)

    nextseat :: SeatMap -> Pos -> Char
    nextseat seatmap pos = let cell    = seatmap M.! pos
                               neighbs = mapMaybe (flip M.lookup seatmap) (neighbours pos)
                               empty   = filter (=='L') neighbs
                               occup   = filter (=='#') neighbs
                           in  case cell of
                                 'L' -> if null occup        then '#' else 'L'
                                 '#' -> if length occup >= 4 then 'L' else '#'
                                 x   -> x

    nextmap :: SeatMap -> SeatMap
    nextmap seatmap = M.fromList [ (pos, nextseat seatmap pos) | pos <- M.keys seatmap ]



calc2 :: Input -> Output
calc2 seatmap = Output result
  where
    result = 0



{- Operations -}

part1 :: String -> String
part1 = show . calc1 . parse
part2 = show . calc2 . parse

