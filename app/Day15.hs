module Day15 (day15) where
import Part (Part (Part1, Part2))

import Data.List (nub, sort)
import Text.Parsec

type Parsed = [((Int,Int),(Int,Int))]

parseReportLine = do string "Sensor at x="
                     sensorx <- many1 (char '-' <|> digit)
                     string ", y="
                     sensory <- many1 (char '-' <|> digit)
                     string ": closest beacon is at x="
                     beaconx <- many1 (char '-' <|> digit)
                     string ", y="
                     beacony <- many1 (char '-' <|> digit)
                     pure ((read sensorx, read sensory),(read beaconx, read beacony))

parseInput :: String -> Parsed
parseInput = fmap parseLine . lines
  where parseLine = either (error . show) id . runParser parseReportLine () "none"

distance (x1,y1) (x2,y2) = abs (y2 - y1) + abs (x2 - x1)

countBeaconAbsencePositionsOnRow targetRow scans = (sum . fmap (\(a,b) -> b - a + 1) . combineRanges . sort . concat . fmap getRanges $ scans) - scansOnRow
  where
    getRanges (sensor@(sensorx,sensory),beacon@(beaconx,beacony))
      | sensory > targetRow && sensory - targetRow <= distance sensor beacon
          = let xd = distance sensor beacon - (sensory - targetRow)
             in [(sensorx - xd, sensorx + xd)]
      | sensory < targetRow && targetRow - sensory <= distance sensor beacon
          = let xd = distance sensor beacon - (targetRow - sensory)
             in [(sensorx - xd, sensorx + xd)]
      | otherwise = []
    combineRanges ((a,b):(c,d):ranges)
      | c > a && c <= b+1 = combineRanges ((a,max b d):ranges)
      | otherwise = (a,b):(combineRanges ((c,d):ranges))
    combineRanges x = x
    scansOnRow = length . nub . map (fst . snd) . filter ((== targetRow) . snd . snd) $ scans

part1 :: Parsed -> IO ()
part1 input = do
  putStrLn . show . countBeaconAbsencePositionsOnRow 10 $ input
  putStrLn . show . countBeaconAbsencePositionsOnRow 2000000 $ input

part2 :: Parsed -> IO ()
part2 _ = putStrLn "part2"

day15 part args = do
  let filename = case args of
                   [] -> "inputs/day15"
                   [f] -> f
  inputs <- parseInput <$> readFile filename
  case part of
    Part1 -> part1 inputs
    Part2 -> part2 inputs
