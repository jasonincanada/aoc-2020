module Day03 (calc1, calc2, part1, part2, parse) where

{-  Advent of Code 2020 - Day 3 - https://adventofcode.com/2020/day/3 -}


{- Types -}

type Forest   = [[Char]]

type Input    = Forest
data Output   = Output Int

instance Show Output where
  show (Output result) = show result


{- Parsing -}

parse :: String -> Input
parse = lines


{- Methods -}

calc1 :: Input -> Output
calc1 forest = Output hits
  where
    hits    = length $ filter (=='#') hops
    hops    = zipWith (!!) forest offsets
    offsets = map ((`mod` width) . (*3)) [0..]
    width   = length $ head forest


-- same as calc1 but generalized a bit so we can try multiple slopes
calc2 :: Input -> Output
calc2 forest = Output result
  where
    result = product $ map (length . trees) paths
    trees  = filter (=='#')
    paths  = map toboggan slopes
    slopes = [ (1,1), (3,1), (5,1), (7,1), (1,2) ]   -- given in the problem description
    
    -- return the path of characters hit while tobogganing a particular slope
    toboggan :: (Int,Int) -> [Char]
    toboggan (right,down) = zipWith (!!) trimmed offsets
      where
        trimmed = every down forest
        offsets = map ((`mod` width) . (*right)) [0..]
        width   = length $ head forest

        -- https://stackoverflow.com/questions/2026912/how-to-get-every-nth-element-of-an-infinite-list-in-haskell
        every n [] = []
        every n as = head as : every n (drop n as)



{- Operations -}

part1 :: String -> String
part1 = show . calc1 . parse
part2 = show . calc2 . parse

