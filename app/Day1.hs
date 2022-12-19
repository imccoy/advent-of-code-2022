module Day1 (day1) where
import Part (Part (Part1, Part2))
import Data.List (sort)
import Data.List.Split (splitWhen)

type Parsed = [[Int]]

parseInput :: String -> Parsed
parseInput = map (map read) . splitWhen (== "") . lines

part1 :: Parsed -> IO ()
part1 = putStrLn . show . maximum . fmap sum

part2 :: Parsed -> IO ()
part2 = putStrLn . show . sum . take 3 . reverse . sort . fmap sum

day1 part args = do let filename = (case args of
                                      [] -> "inputs/day1"
                                      [f] -> f)
                    inputs <- parseInput <$> readFile filename
                    case part of
                      Part1 -> part1 inputs
                      Part2 -> part2 inputs
