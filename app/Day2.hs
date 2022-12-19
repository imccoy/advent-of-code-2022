module Day2 (day2) where
import Part (Part (Part1, Part2))
import Data.List.Split (splitWhen)

data Move = Rock | Paper | Scissors

oppMove 'A' = Rock
oppMove 'B' = Paper
oppMove 'C' = Scissors

myMove1 'X' = Rock
myMove1 'Y' = Paper
myMove1 'Z' = Scissors

shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

data Outcome = Win | Lose | Draw

outcomeScore Win = 6
outcomeScore Draw = 3
outcomeScore Lose = 0

type Parsed = [(Char,Char)]

parseInput :: String -> Parsed
parseInput = (map parseRound) . lines
  where parseRound (a:' ':b:[]) = (a, b)

outcome Rock Scissors = Lose
outcome Rock Paper = Win
outcome Paper Rock = Lose
outcome Paper Scissors = Win
outcome Scissors Paper = Lose
outcome Scissors Rock = Win
outcome _ _ = Draw

roundScore opp mine = outcomeScore (outcome opp mine) + shapeScore mine

part1 :: Parsed -> IO ()
part1 = putStrLn . show . sum . map (\(a, b) -> roundScore (oppMove a) (myMove1 b))

desiredOutcome 'X' = Lose
desiredOutcome 'Y' = Draw
desiredOutcome 'Z' = Win

myMove2 Rock Win = Paper
myMove2 Rock Lose = Scissors
myMove2 Paper Win = Scissors
myMove2 Paper Lose = Rock
myMove2 Scissors Win = Rock
myMove2 Scissors Lose = Paper
myMove2 x Draw = x

part2 :: Parsed -> IO ()
part2 = putStrLn . show . sum . map (\(a, b) -> let a' = oppMove a
                                                    b' = desiredOutcome b
                                                 in roundScore a' (myMove2 a' b'))

day2 part args = do let filename = case args of
                                      [] -> "inputs/day2"
                                      [f] -> f
                    inputs <- parseInput <$> readFile filename
                    case part of
                      Part1 -> part1 inputs
                      Part2 -> part2 inputs
