module Main where

import Day06 (part1, part2)

main :: IO ()
main = do
  input <- readFile "inputs/6.txt"
  putStrLn $ part1 input
  putStrLn $ part2 input

