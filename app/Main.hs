module Main where

import Day22 (part1, part2)

main :: IO ()
main = do
  input <- readFile "inputs/22.txt"
  putStrLn $ part1 input
  putStrLn $ part2 input

