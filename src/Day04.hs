module Day04 where

{-  Advent of Code 2020 - Day 4 - https://adventofcode.com/2020/day/4 -}

import Control.Applicative  ((<|>))
import Control.Monad        (guard, replicateM)
import Data.List.Split      (splitOn)
import Data.Maybe           (mapMaybe)
import NanoParsec


{- Types -}

data Field    = Byr Int       -- birth year
              | Iyr Int       -- issue year
              | Eyr Int       -- expiration year
              | Hgt Int Units -- height
              | Hcl String    -- hair color
              | Ecl String    -- eye color
              | Pid String    -- passport id
              | Cid Int       -- country id
              deriving (Eq, Show)

data Units    = CM
              | IN
              deriving (Eq, Show)

data Output   = Output Int

instance Show Output where
  show (Output count) = show count


{- Parsing/Validation -}

-- return passport as list of [key,value] pairs with no validation
parse1 :: String -> [[[String]]]
parse1 = map (map (splitOn ":") . concatMap words)
          . splitOn [""]
          . lines

-- parse valid passport fields only and return as many as we find
parse2 :: String -> [[Field]]
parse2 = map (mapMaybe field . concatMap words)
          . splitOn [""]
          . lines
  where
    field :: String -> Maybe Field
    field = try $  birthYear
               <|> issueYear
               <|> expirYear
               <|> height
               <|> hairColor
               <|> eyeColor
               <|> passport
               <|> country


birthYear :: Parser Field
birthYear = Byr <$> (string "byr:" >> between 1920 2002)
issueYear = Iyr <$> (string "iyr:" >> between 2010 2020)
expirYear = Eyr <$> (string "eyr:" >> between 2020 2030)

between :: Int -> Int -> Parser Int
between from to = do
  num <- number
  guard  $ num `elem` [from..to]
  return $ num

height :: Parser Field
height = string "hgt:" >> (cm <|> inches)
  where
    cm      = Hgt <$> between 150 193 <* string "cm" <*> pure CM
    inches  = Hgt <$> between 59  76  <* string "in" <*> pure IN
  
hairColor :: Parser Field
hairColor = Hcl <$> (string "hcl:#" >> replicateM 6 hex)
  where
    hex = oneOf "0123456789abcdef"

eyeColor :: Parser Field
eyeColor = do
  string "ecl:"
  color <- replicateM 3 item
  guard  $ color `elem` words "amb blu brn gry grn hzl oth"
  return $ Ecl color

passport :: Parser Field
passport = string "pid:" >> Pid <$> replicateM 9 digit
country  = string "cid:" >> Cid <$> number


{- Methods -}

calc1 :: [[[String]]] -> Output
calc1 passports = Output count
  where
    count = length $ filter valid passports

    valid :: [[String]] -> Bool
    valid passport = all (`elem` fields) required
      where
        fields   = map head passport
        required = words "byr iyr eyr hgt hcl ecl pid"


calc2 :: [[Field]] -> Output
calc2 passports = Output count
  where
    count = length $ filter valid passports

    valid :: [Field] -> Bool
    valid passport = all (`elem` fields) required
      where
        fields   = map code passport
        required = words "byr iyr eyr hgt hcl ecl pid"

        code :: Field -> String
        code (Byr _  ) = "byr"
        code (Iyr _  ) = "iyr"
        code (Eyr _  ) = "eyr"
        code (Hgt _ _) = "hgt"
        code (Hcl _  ) = "hcl"
        code (Ecl _  ) = "ecl"
        code (Pid _  ) = "pid"
        code (Cid _  ) = "cid"


{- Operations -}

part1 :: String -> String
part1 = show . calc1 . parse1
part2 = show . calc2 . parse2

