module Day3 (day3, priority) where
import Part (Part (Part1, Part2))
import Data.List.Split (chunksOf)

type Parsed = [String]

parseInput :: String -> Parsed
parseInput = lines

rucksackParts line = let numPerPart = length line `div` 2
                      in (take numPerPart line, drop numPerPart line)

priority c | c >= 'A' && c <= 'Z' = fromEnum c - fromEnum 'A' + 27
           | c >= 'a' && c <= 'z' = fromEnum c - fromEnum 'a' + 1

part1 :: Parsed -> IO ()
part1 = putStrLn . show . sum . map (priority . sharedItem . rucksackParts)

sharedItem = head . uncurry sharedItems
sharedItems a b = filter (\c -> elem c b) $ a

part2 :: Parsed -> IO ()
part2 = putStrLn . show . sum . map (priority . head . foldr1 sharedItems) . chunksOf 3

day3 part args = do let filename = case args of
                                      [] -> "inputs/day3"
                                      [f] -> f
                    inputs <- parseInput <$> readFile filename
                    case part of
                      Part1 -> part1 inputs
                      Part2 -> part2 inputs
