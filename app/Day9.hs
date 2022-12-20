module Day9 (day9) where

import Data.Set (Set)
import qualified Data.Set as Set

import Part (Part (Part1, Part2))

data Direction = DUp | DRight | DDown | DLeft
  deriving (Show)

offsetDirection DUp = (0,1)
offsetDirection DRight = (1,0)
offsetDirection DDown = (0,-1)
offsetDirection DLeft = (-1, 0)

parseDirection 'U' = DUp
parseDirection 'R' = DRight
parseDirection 'D' = DDown
parseDirection 'L' = DLeft

type Parsed = [(Direction, Int)]

data Locations = Locations { headLocation :: (Int,Int), tailLocation :: (Int, Int) }

moveInDirection dir (x,y) = (x + xOff, y + yOff)
 where (xOff, yOff) = offsetDirection dir

chaseHead (headx, heady) (tailx, taily)
  | abs (headx - tailx) <= 1 && abs (heady - taily) <= 1 = (tailx, taily)
  | otherwise = let xoff = signum (headx - tailx)
                    yoff = signum (heady - taily)
                 in (tailx + xoff, taily + yoff)

step locations dir = let newHeadLocation = moveInDirection dir (headLocation locations)
                         newTailLocation = chaseHead newHeadLocation (tailLocation locations)
                      in Locations { headLocation = newHeadLocation, tailLocation = newTailLocation }

runSteps :: [(Direction, Int)] -> Set (Int, Int)
runSteps = go (Set.singleton (0,0)) (Locations (0,0) (0,0))
  where go tailSites _ [] = tailSites
        go tailSites locations ((_,0):commands) = go tailSites locations commands
        go tailSites locations ((dir,distance):commands) = let locations' = step locations dir
                                                               tailSites' = Set.insert (tailLocation locations') tailSites
                                                            in go tailSites' locations' ((dir,distance-1):commands)

parseLine (d:' ':s) = (parseDirection d, read s)

parseInput :: String -> Parsed
parseInput = fmap parseLine . lines

part1 :: Parsed -> IO ()
part1 = putStrLn . show . Set.size . runSteps

stepn (headLocation:tailLocations) dir = let newHeadLocation = moveInDirection dir headLocation
                                             chase tailLocation [] = (tailLocation, [tailLocation])
                                             chase newHeadLocation (tailLocation:tailLocations) = let tailLocation' = chaseHead newHeadLocation tailLocation
                                                                                                      (finalTail, tailLocations') = chase tailLocation' tailLocations
                                                                                                   in (finalTail, newHeadLocation:tailLocations')
                                          in chase newHeadLocation tailLocations

runStepsN :: Int -> [(Direction, Int)] -> Set (Int, Int)
runStepsN n = go (Set.singleton (0,0)) (replicate n (0,0))
  where go tailSites _ [] = tailSites
        go tailSites locations ((_,0):commands) = go tailSites locations commands
        go tailSites locations ((dir,distance):commands) = let (tailLocation, locations') = stepn locations dir
                                                               tailSites' = Set.insert tailLocation tailSites
                                                            in go tailSites' locations' ((dir,distance-1):commands)


part2 :: Parsed -> IO ()
part2 input = do
  putStrLn . show . Set.size . runStepsN 2 $ input
  putStrLn . show . Set.size . runStepsN 10 $ input

day9 part args = do let filename = case args of
                                      [] -> "inputs/day9"
                                      [f] -> f
                    inputs <- parseInput <$> readFile filename
                    case part of
                      Part1 -> part1 inputs
                      Part2 -> part2 inputs
