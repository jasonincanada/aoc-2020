module Main where

import Day25 (part1, part2)

main :: IO ()
main = do
  input <- readFile "inputs/25.txt"
  putStrLn $ part1 input
  putStrLn $ part2 input

