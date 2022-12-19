module Day4 (day4) where
import Part (Part (Part1, Part2))
import Data.List.Split (splitOn)

type Parsed = [((Int,Int),(Int,Int))]

parseInput :: String -> Parsed
parseInput = map (listTuple . map readRange . splitOn ",")  . lines
  where readRange :: String -> (Int,Int)
        readRange = listTuple . map read . splitOn "-"
        listTuple [a,b] = (a, b)

fullyContained ((aMin, aMax),(bMin, bMax))
  | aMin <= bMin && aMax >= bMax = True
  | bMin <= aMin && bMax >= aMax = True
  | otherwise                    = False

part1 :: Parsed -> IO ()
part1 = putStrLn . show . length . filter fullyContained


overlapping ((aMin, aMax),(bMin, bMax))
  | aMax >= bMin && aMin <= bMax = True
  | bMax >= aMin && bMin <= aMax = True
  | otherwise                    = False

part2 :: Parsed -> IO ()
part2 = putStrLn . show . length . filter overlapping

day4 part args = do let filename = case args of
                                      [] -> "inputs/day4"
                                      [f] -> f
                    inputs <- parseInput <$> readFile filename
                    case part of
                      Part1 -> part1 inputs
                      Part2 -> part2 inputs
