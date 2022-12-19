module Day6 (day6) where
import Part (Part (Part1, Part2))

type Parsed = String

parseInput :: String -> Parsed
parseInput = id

findPacketStart n (b1:b2:b3:b4:bs) | b1 /= b2 && b1 /= b3 && b1 /= b4 && b2 /= b3 && b2 /= b4 && b3 /= b4 = n + 4
                                   | otherwise = findPacketStart (n+1) (b2:b3:b4:bs)

part1 :: Parsed -> IO ()
part1 = putStrLn . show . findPacketStart 0

findChunkStart size bytes = go size (take size bytes) (drop size bytes)
  where go n lead rest | allUnique lead = n
                       | otherwise = go (n+1) (tail lead ++ [head rest]) (tail rest)

allUnique [] = True
allUnique (b:bs) = notElem b bs && allUnique bs

part2 :: Parsed -> IO ()
part2 bytes = do putStrLn . show . findPacketStart 0 $ bytes
                 putStrLn . show . findChunkStart 4 $ bytes 
                 putStrLn . show . findChunkStart 14 $ bytes 

day6 part args = do let filename = case args of
                                      [] -> "inputs/day6"
                                      [f] -> f
                    inputs <- parseInput <$> readFile filename
                    case part of
                      Part1 -> part1 inputs
                      Part2 -> part2 inputs
