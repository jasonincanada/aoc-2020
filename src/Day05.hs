module Day05 where

{-  Advent of Code 2020 - Day 5 - https://adventofcode.com/2020/day/5 -}


{- Types -}

type Pass    = [Char]

type Input   = [Pass]
data Output  = Output Int

instance Show Output where
  show (Output count) = show count


{- Parsing -}

parse :: String -> Input
parse = lines


{- Methods -}

decode :: Pass -> Int
decode pass = seatID
  where
    seatID = row*8 + col

    row    = go (take 7 pass) (0, 127)
    col    = go (drop 7 pass) (0, 7  )

    go :: [Char] -> (Int,Int) -> Int
    go []     (n  ,_   ) = n
    go (c:cs) (low,high)
      | c `elem` "FL"  = go cs (low     , high-dist)
      | c `elem` "BR"  = go cs (low+dist, high     )
      where
        dist = (high-low+1) `div` 2



calc1 :: Input -> Output
calc1 passes = Output count
  where
    count = maximum $ map decode passes


calc2 :: Input -> Output
calc2 passes = Output mySeat
  where
    ids     = map decode passes
    lowest  = minimum ids
    highest = maximum ids

    mySeat  = head [ n | n <- [lowest..highest],

                         (n-1) `elem`    ids,
                         (n  ) `notElem` ids,
                         (n+1) `elem`    ids ]



{- Operations -}

part1 :: String -> String
part1 = show . calc1 . parse
part2 = show . calc2 . parse

