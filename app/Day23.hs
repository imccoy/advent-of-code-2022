module Day23 (day23) where

import Data.Maybe (catMaybes, isJust)
import Data.List (intersect)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Part (Part (Part1, Part2))

type Parsed = Set (Int,Int)

parseInput :: String -> Parsed
parseInput input = parseMap (zip [0..] . map (zip [0..]) $ lines input)
  where parseMap mapWithCoords = Set.fromList [ (x, y)
                                              | (y, cols) <- mapWithCoords
                                              , (x, char) <- cols
                                              , char == '#'
                                              ]

data Heading = HeadingN | HeadingNE | HeadingE | HeadingSE | HeadingS | HeadingSW | HeadingW | HeadingNW
  deriving (Show, Eq, Enum)
allHeadings = [HeadingN .. HeadingNW]

offsetXY HeadingN = (0,-1)
offsetXY HeadingNE = (1,-1)
offsetXY HeadingE = (1,0)
offsetXY HeadingSE = (1,1)
offsetXY HeadingS = (0,1)
offsetXY HeadingSW = (-1,1)
offsetXY HeadingW = (-1,0)
offsetXY HeadingNW = (-1,-1)

offsetStep heading (x,y) = let (x',y') = offsetXY heading
                            in (x + x', y + y')


relatedHeadings HeadingN = [HeadingNW, HeadingN, HeadingNE]
relatedHeadings HeadingE = [HeadingNE, HeadingE, HeadingSE]
relatedHeadings HeadingS = [HeadingSE, HeadingS, HeadingSW]
relatedHeadings HeadingW = [HeadingSW, HeadingW, HeadingNW]

propose elves directions = [ if null neighbours
                               then ((x,y), Nothing)
                               else ((x,y), case filter (\direction -> null . intersect neighbours $ relatedHeadings direction) directions of
                                              [] -> Nothing
                                              (h:_) -> Just $ offsetStep h (x,y)
                                    )
                           | (x,y) <- Set.toList elves
                           , let neighbours = filter (\heading -> Set.member (offsetStep heading (x,y)) elves) allHeadings
                           ]

move :: [((Int,Int), Maybe (Int, Int))] -> Maybe (Set (Int,Int))
move proposals = let destinationCounts = foldr (Map.unionWith (+)) Map.empty . map (\x -> Map.fromList [(x,1)]) . catMaybes . map snd $ proposals
                     successfulProposals = Map.filter (== 1) destinationCounts
                     destinationProposals = [ (elf, proposal >>= (\elf' -> if Map.member elf' successfulProposals
                                                                             then Just elf'
                                                                             else Nothing))
                                            | (elf,proposal) <- proposals
                                            ]
                  in if null $ filter (isJust . snd) destinationProposals
                       then Nothing
                       else Just $ Set.fromList [case proposal of
                                                   Just elf' -> elf'
                                                   Nothing -> elf
                                                | (elf,proposal) <- destinationProposals
                                                ]
initialDirections = [HeadingN, HeadingS, HeadingW, HeadingE]
rotate (x:xs) = xs ++ [x]

rounds elves directions = let proposals = propose elves directions
                           in elves:(case move proposals of
                                       Just elves' -> rounds elves' (rotate directions)
                                       Nothing -> [])


countEmpties elves = sum [if elem (x,y) elves then 0 else 1 | x <- [minX..maxX], y <- [minY..maxY]]
  where
    minX = minimum . map fst $ Set.toList elves
    maxX = maximum . map fst $ Set.toList elves
    minY = minimum . map snd $ Set.toList elves
    maxY = maximum . map snd $ Set.toList elves

printElves :: Set (Int,Int) -> IO ()
printElves elves = putStrLn $ unlines [[if elem (x,y) elves then '#' else '.' | x <- [minX..maxX] ] | y <- [minY..maxY]]
  where
    minX = minimum . map fst $ Set.toList elves
    maxX = maximum . map fst $ Set.toList elves
    minY = minimum . map snd $ Set.toList elves
    maxY = maximum . map snd $ Set.toList elves


part1 :: Parsed -> IO ()
part1 elves = putStrLn . show . countEmpties . head . drop 10 . rounds elves $ initialDirections

part2 :: Parsed -> IO ()
part2 elves = putStrLn . show . length . rounds elves $ initialDirections

day23 part args = do
  let filename = case args of
                   [] -> "inputs/day23"
                   [f] -> f
  inputs <- parseInput <$> readFile filename
  case part of
    Part1 -> part1 inputs
    Part2 -> part2 inputs
