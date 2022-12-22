module Day15 (day15) where
import Part (Part (Part1, Part2))

import Control.Monad (forM_)
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

unknownBeaconAbsencePositionsOnRow targetRow scans = combineRanges . sort . concat . fmap getRanges $ scans
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

part1 :: Parsed -> IO ()
part1 input = do
  let scansOnRow = length . nub . map (fst . snd) . filter ((== 2000000) . snd . snd) $ input
  putStrLn . show $ (sum . fmap (\(a,b) -> b - a + 1) . unknownBeaconAbsencePositionsOnRow 2000000 $ input) - scansOnRow

gapOfOne min max ((_,b):(ranges@((c,_):_)))
  | b > min && b + 2 == c = Just $ b + 1
  | b > max = Nothing
  | otherwise = gapOfOne min max ranges
gapOfOne _ _ _ = Nothing

part2 :: Parsed -> IO ()
part2 input = do
  forM_ [0..4000000] $ \y -> do
    case gapOfOne 0 4000000 (unknownBeaconAbsencePositionsOnRow y input) of
      Just x -> putStrLn $ "FOUND IT" ++ show (x * 4000000 + y)
      Nothing -> pure ()
    if y `mod` 100000 == 0 then putStrLn (show y) else pure ()


day15 part args = do
  let filename = case args of
                   [] -> "inputs/day15"
                   [f] -> f
  inputs <- parseInput <$> readFile filename
  case part of
    Part1 -> part1 inputs
    Part2 -> part2 inputs
