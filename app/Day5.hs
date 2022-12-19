module Day5 (day5) where

import Part (Part (Part1, Part2))

import Data.List.Split (splitOn)
import Data.List (intercalate, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Safe (atMay)

type Stacks = Map Int [Char]
data Move = Move { moveCount :: Int, moveFrom :: Int, moveTo :: Int }
  deriving (Show)

type Parsed = (Stacks,[Move])

parseInput :: String -> Parsed
parseInput input = let (crateRowLines, (labelRow:_:moveLines)) = span (elem '[') $ lines input
                    in (parseRows labelRow crateRowLines, parseMoves moveLines)

parseRows labelRow crateRowLines = foldr (insertCrateRow labelRow) Map.empty crateRowLines
  where insertCrateRow :: String -> String -> Map Int [Char] -> Map Int [Char]
        insertCrateRow labelRow crateRow stack = foldr insertCrate stack (zip labelRow crateRow)
        insertCrate :: (Char,Char) -> Map Int [Char] -> Map Int [Char]
        insertCrate (' ',_) stacks = stacks
        insertCrate (_,' ') stacks = stacks
        insertCrate (stackNumber,crateLabel) stacks = Map.alter (Just . (crateLabel:) . fromMaybe [])
                                                                (read [stackNumber]) stacks

parseMoves = map parseMove
  where parseMove line = let ["move",count,"from",from,"to",to] = splitOn " " line
                          in Move { moveCount = read count
                                  , moveFrom = read from
                                  , moveTo = read to
                                  }

runMove :: Move -> Stacks -> Stacks
runMove (Move { moveCount = 0 }) stacks = stacks
runMove move stacks = runMove (move { moveCount = (moveCount move) - 1 })
                              (move1 move stacks)

stackHead stacks number = head $ ((Map.!) stacks number)

move1 :: Move -> Stacks -> Stacks
move1 move stacks = let crate = stackHead stacks (moveFrom move)
                     in Map.adjust (crate:) (moveTo move) $
                        Map.adjust tail (moveFrom move) $
                        stacks

runMoves f [] stacks = pure stacks
runMoves f (move:moves) stacks = do let next = f move stacks
                                    printStacks next
                                    runMoves f moves next

printStacks stacks = let height = maximum . map length . Map.elems $ stacks
                         labels = sort . Map.keys $ stacks
                      in putStrLn . unlines . reverse $ 
                         (intercalate " " $ map (\x -> " " ++ show x ++ " ") labels):
                         [ intercalate " " $ map (\x -> case atMay ((Map.!) stacks x) (height - row) of
                                                          Nothing -> "   "
                                                          Just c -> "[" ++ [c] ++ "]") labels
                         | row <- [0..height]]

part1 :: Parsed -> IO ()
part1 (stacks, moves) = do printStacks stacks
                           finished <- runMoves runMove moves stacks
                           putStrLn $ show $ map (stackHead finished) [1..9]

runMovePart2 :: Move -> Stacks -> Stacks
runMovePart2 move stacks = let crates = take (moveCount move) ((Map.!) stacks (moveFrom move))
                            in Map.adjust (crates ++) (moveTo move) $
                               Map.adjust (drop (moveCount move)) (moveFrom move) $
                               stacks



part2 :: Parsed -> IO ()
part2 (stacks, moves) = do printStacks stacks
                           finished <- runMoves runMovePart2 moves stacks
                           putStrLn $ show $ map (stackHead finished) [1..9]

day5 part args = do let filename = case args of
                                      [] -> "inputs/day5"
                                      [f] -> f
                    inputs <- parseInput <$> readFile filename
                    case part of
                      Part1 -> part1 inputs
                      Part2 -> part2 inputs
