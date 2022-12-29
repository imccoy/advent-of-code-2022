module Day25 (day25) where

import Part (Part (Part1, Part2))

type Parsed = [String]

fubarDigitValue '2' = 2
fubarDigitValue '1' = 1
fubarDigitValue '0' = 0
fubarDigitValue '-' = -1
fubarDigitValue '=' = -2

fubarToInt :: String -> Integer
fubarToInt = go 1 . reverse
  where go p (d:ds) = fubarDigitValue d * p + go (p*5) ds
        go _ [] = 0

intToFubar :: Integer -> String
intToFubar = reverse . go
  where go 0 = ""
        go n | (n - 2) `mod` 5 == 0 = '2':(go $ (n-2) `div` 5)
             | (n - 1) `mod` 5 == 0 = '1':(go $ (n-1) `div` 5)
             | n       `mod` 5 == 0 = '0':(go $  n    `div` 5)
             | (n + 1) `mod` 5 == 0 = '-':(go $ (n+1) `div` 5)
             | (n + 2) `mod` 5 == 0 = '=':(go $ (n+2) `div` 5)

parseInput :: String -> Parsed
parseInput = lines

part1 :: Parsed -> IO ()
part1 nums = do
  putStrLn . intToFubar . sum . map fubarToInt $ nums

part2 :: Parsed -> IO ()
part2 _ = putStrLn "part2"

day25 part args = do
  let filename = case args of
                   [] -> "inputs/day25"
                   [f] -> f
  inputs <- parseInput <$> readFile filename
  case part of
    Part1 -> part1 inputs
    Part2 -> part2 inputs
