{-# Language TupleSections #-}

module Day11 where

{-  Advent of Code 2020 - Day 11 - https://adventofcode.com/2020/day/11 -}

import qualified Data.Map.Strict as M
import Data.Bifunctor (bimap, first)
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
    result = count 4 getneighbours seatmap

    getneighbours :: SeatMap -> Pos -> [Kind]
    getneighbours seatmap pos = mapMaybe (`M.lookup` seatmap) (neighbours pos)

    neighbours :: Pos -> [Pos]
    neighbours (row,col) = bimap (+row) (+col) <$> offsets
      where
        offsets = [ (-1,-1), (-1,0), (-1,1),
                    ( 0,-1),         ( 0,1),
                    ( 1,-1), ( 1,0), ( 1,1) ]


type Ray = (Int->Int, Int->Int)

calc2 :: Input -> Output
calc2 seatmap = Output result
  where
    result = count 5 hits seatmap

    hits :: SeatMap -> Pos -> [Kind]
    hits seatmap pos = mapMaybe (go pos) rays
      where
        go :: Pos -> Ray -> Maybe Kind
        go pos (fr,fc) = let next = bimap fr fc pos
                         in  case M.lookup next seatmap of
                               Nothing  -> Nothing
                               Just '.' -> go next (fr,fc)
                               Just  x  -> Just x

        rays :: [Ray]
        rays = [ (sub,sub), (sub,nop), (sub,add),
                 (nop,sub),            (nop,add),
                 (add,sub), (add,nop), (add,add) ]
          where
            sub = subtract 1
            add = (+1)
            nop = (+0)


-- keep iterating a function on a value until it doesn't change from its previous value,
-- then run a finalizing function on the last value
recurse :: Eq a => (a -> a) -> (a -> b) -> a -> b
recurse next final a
  | a == a'   = final a
  | otherwise = recurse next final a'
  where
    a' = next a


count :: Int -> (SeatMap -> Pos -> [Kind]) -> SeatMap -> Int
count l getneighbours = recurse nextmap final
  where
    nextmap :: SeatMap -> SeatMap
    nextmap seatmap = M.fromList [ (pos, nextseat seatmap pos) | pos <- M.keys seatmap ]

    nextseat :: SeatMap -> Pos -> Char
    nextseat seatmap pos = let cell    = seatmap M.! pos
                               neighbs = getneighbours seatmap pos
                               empty   = filter (=='L') neighbs
                               occup   = filter (=='#') neighbs
                           in  case cell of
                                 'L' -> if null occup        then '#' else 'L'
                                 '#' -> if length occup >= l then 'L' else '#'
                                 x   -> x

    -- count the number of occupied seats
    final :: SeatMap -> Int
    final = M.size . M.filter (=='#')


{- Operations -}

part1 :: String -> String
part1 = show . calc1 . parse
part2 = show . calc2 . parse

