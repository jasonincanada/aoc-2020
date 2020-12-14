module Main where

import Day12 (part1, part2)

main :: IO ()
main = do
  input <- readFile "inputs/12.txt"
  putStrLn $ part1 input
  putStrLn $ part2 input

