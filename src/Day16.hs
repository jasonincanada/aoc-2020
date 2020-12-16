module Day16 where

{-  Advent of Code 2020 - Day 16 - https://adventofcode.com/2020/day/16 -}

import Data.Bifunctor    (bimap)
import Data.List.Split   (splitOn)
import NanoParsec hiding (parse)


{- Types -}

type Value   = Int
type Range   = (Value, Value)
type Field   = (String, [Range])

type Ticket  = [Value]

-- list of fields, my ticket, nearby tickets
type Input   = ([Field], Ticket, [Ticket])

data Output  = Output Int

instance Show Output where
  show (Output result) = show result


{- Parsing -}

parse :: String -> Input
parse file = (fields, myticket, nearby)
  where
    (c1:c2:c3:_) = splitOn [""] $ lines file

    myticket = head . map csv . drop 1 $ c2
    nearby   =        map csv . drop 1 $ c3
    csv      = map read . splitOn ","

    fields   = run field <$> c1

    -- departure location: 40-152 or 161-969
    field :: Parser Field
    field = do
      name   <- upto ':' <* space
      range1 <- range    <* string " or "
      range2 <- range
      return $ (name, [range1,range2])

    -- 40-152
    range :: Parser Range
    range = (,) <$> number <* char '-'
                <*> number



{- Methods -}

calc1 :: Input -> Output
calc1 (fields, _, nearby) = Output result
  where
    result    = sum . filter invalid . concat $ nearby
    invalid v = not $ any (inRange v) ranges
    ranges    = concatMap snd fields

    inRange :: Value -> Range -> Bool
    inRange val = uncurry (&&) . bimap (val >=) (val <=)


calc2 :: Input -> Output
calc2 input = Output result
  where
    result = 0



{- Operations -}

part1 :: String -> String
part1 = show . calc1 . parse
part2 = show . calc2 . parse

