module Day07 where

{-  Advent of Code 2020 - Day 7 - https://adventofcode.com/2020/day/7 -}

import qualified Data.Set as S


{- Types -}

type Color   = String
data Bag     = Bag Color [(Int,Color)]

type Input   = [Bag]
data Output  = Output Int

instance Show Output where
  show (Output count) = show count


{- Parsing -}

-- light blue bags contain 2 light chartreuse bags, 4 drab black bags, 2 dull bronze bags.
-- drab black bags contain no other bags.
parse :: String -> Input
parse = map (bag . words . clean) . lines
  where
    clean = filter (`notElem` ".,")   -- remove periods and commas to simplify parsing

    bag :: [String] -> Bag
    bag (a:c:"bags":"contain":contents) = Bag (a++" "++c) (fill contents)

    fill :: [String] -> [(Int,Color)]
    fill []                       = []
    fill ("no":"other":"bags":[]) = []
    fill (n:a:c:_:rest)           = (read n, a++" "++c) : fill rest



{- Methods -}

calc1 :: Input -> Output
calc1 bags = Output count
  where
    count = length $ parents ["shiny gold"]

    parents :: [Color] -> [Color]
    parents colors = let these = dedupe $ concatMap f colors
                     in  if null these
                         then []
                         else these ++ dedupe (parents these)

    -- get the list of colors that contain this one
    f :: Color -> [Color]
    f color = map getcolor $ filter (contains color) bags
      where
        contains :: Color -> Bag -> Bool
        contains color (Bag _ cont) = color `elem` map snd cont

        getcolor :: Bag -> Color
        getcolor (Bag c _) = c

    dedupe :: Ord a => [a] -> [a]
    dedupe = S.toList . S.fromList



calc2 :: Input -> Output
calc2 bags = Output $ from "shiny gold" - 1
  where
    from :: Color -> Int
    from color = 1 + sum [ n * from c | (n, c) <- subs ]
      where
        Bag _ subs = get color

    get :: Color -> Bag
    get color = head $ filter (\(Bag c _) -> c == color) bags



{- Operations -}

part1 :: String -> String
part1 = show . calc1 . parse
part2 = show . calc2 . parse

