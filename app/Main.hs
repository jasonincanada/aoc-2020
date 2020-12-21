module Main where

import Day20 (part1, part2)

main :: IO ()
main = do
  input <- readFile "inputs/20.txt"
  putStrLn $ part1 input
  putStrLn $ part2 input

