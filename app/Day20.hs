module Day20 (day20) where

import Control.Monad (forM_)
import Data.List (findIndex, (!!))
import Data.Maybe (fromJust)
import Debug.Trace

import Part (Part (Part1, Part2))

type Parsed = [(Int, Integer)]

parseInput :: String -> Parsed
parseInput = zip [0..] . fmap read . lines

mix numbers = go 0 numbers
  where go n order | n >= length numbers = order
                   | otherwise = let (currentValue@(_, shiftBy), rest) = focus n order
                                     numbers' = shiftN (normalize shiftBy) currentValue rest
                                  in --trace (show (map snd order, currentValue, map snd rest, map snd numbers')) $ 
                                     go (n+1) numbers'
        normalize v = v `mod` (fromIntegral (length numbers) - 1)
        focus index elems = let (before, (elem:after)) = span (\(n,_) -> n /= index) elems
                             in (elem, after ++ before)
        shiftN distance elem elems = let (before,after) = splitAt (fromIntegral distance) elems
                                      in before ++ [elem] ++ after


part1 :: Parsed -> IO ()
part1 numbers = do putStrLn $ show $ length numbers
                   let mixed = map snd $ mix numbers
                   let (before0, from0) = span (/= 0) mixed
                   let rotated = cycle $ from0 ++ before0
                   let a = rotated !! 1000
                   let b = rotated !! 2000
                   let c = rotated !! 3000
                   putStrLn $ show (a,b,c)
                   putStrLn $ show $ a + b + c

mixN 0 numbers = numbers
mixN n numbers = mixN (n-1) (mix numbers)

part2 :: Parsed -> IO ()
part2 numbers = do let numbers' = (\(i,n) -> (i, n * 811589153)) <$> numbers
                   let mixed = map snd $ mixN 10 numbers'
                   let (before0, from0) = span (/= 0) mixed
                   let rotated = cycle $ from0 ++ before0
                   let a = rotated !! 1000
                   let b = rotated !! 2000
                   let c = rotated !! 3000
                   putStrLn $ show (a,b,c)
                   putStrLn $ show $ a + b + c

day20 part args = do
  let filename = case args of
                   [] -> "inputs/day20"
                   [f] -> f
  inputs <- parseInput <$> readFile filename
  case part of
    Part1 -> part1 inputs
    Part2 -> part2 inputs
