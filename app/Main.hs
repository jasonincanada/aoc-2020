module Main where

import Day16 (part1, part2)

main :: IO ()
main = do
  input <- readFile "inputs/16.txt"
  putStrLn $ part1 input
  putStrLn $ part2 input

