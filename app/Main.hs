module Main where

import Day10 (part1, part2)

main :: IO ()
main = do
  input <- readFile "inputs/10.txt"
  putStrLn $ part1 input
  putStrLn $ part2 input

