module Day20 where

{-  Advent of Code 2020 - Day 20 - https://adventofcode.com/2020/day/20 -}

import Data.Char       (isDigit)
import Data.Function   ((&))
import Data.List       (delete)
import Data.List.Split (splitOn)

{- Types -}

type Pixel   = Char  -- . or #
type ID      = Int
type Tile    = (ID, [[Pixel]])

type Input   = [Tile]
data Output  = Output Int

instance Show Output where
  show (Output result) = show result


{- Parsing -}

parse :: String -> Input
parse = map tile . splitOn [""] . lines

tile :: [String] -> Tile
tile (name:pixels) = (number, pixels)
  where
    number = filter isDigit name & read



{- Methods -}

type Side = [Pixel]

-- from a tile get its 8 sides (TODO: convert to bitmasks for faster comparisons)
sides :: Tile -> [Side]
sides (_, pixels) = [   top,   bottom,   left,   right,
                      r top, r bottom, r left, r right ]
  where
    top    =     head pixels
    bottom =     last pixels
    left   = map head pixels
    right  = map last pixels

    r      = reverse


-- Assumption: There are 0 or 1 tiles that match a side of a tile, never 2 or more

calc1 :: Input -> Output
calc1 tiles = Output result
  where
    result  = product $ map fst corners
    corners = filter (iscorner . matches) tiles
    
    -- a corner is a tile that has only 2 other matching tiles (the rest will have 3 or 4)
    iscorner :: [Tile] -> Bool
    iscorner matches = length matches == 2

    -- get the tiles which have a side in common with the tile in question
    matches :: Tile -> [Tile]
    matches tile = concat [ filter (ismatch side) others | side <- mysides ]
      where
        mysides = take 4 (sides tile)

        ismatch :: Side -> Tile -> Bool
        ismatch side this = side `elem` sides this

        others :: [Tile]
        others = delete tile tiles


calc2 :: Input -> Output
calc2 input = Output result
  where
    result = 0



{- Operations -}

part1 :: String -> String
part1 = show . calc1 . parse
part2 = show . calc2 . parse

