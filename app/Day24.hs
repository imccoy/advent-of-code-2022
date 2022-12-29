module Day24 (day24) where

import Control.Monad (forM_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)

import Graph (dijkstraAny)
import Part (Part (Part1, Part2))

data Heading = HeadingN | HeadingE | HeadingS | HeadingW
  deriving (Show,Eq,Ord)

allHeadings = [HeadingN,HeadingE,HeadingS,HeadingW]

type Parsed = Map (Int,Int) (Maybe [Heading])

parseInput :: String -> Parsed
parseInput input = parseMap (zip [0..] . map (zip [0..]) $ lines input)
  where parseMap mapWithCoords = Map.fromList [ ((x, y), blizz)
                                              | (y, cols) <- mapWithCoords
                                              , (x, char) <- cols
                                              , char /= '.'
                                              , let blizz = case char of
                                                              '#' -> Nothing
                                                              '^' -> Just [HeadingN]
                                                              '>' -> Just [HeadingE]
                                                              'v' -> Just [HeadingS]
                                                              '<' -> Just [HeadingW]
                                              ]

offsetXY HeadingN = (0,-1)
offsetXY HeadingE = (1,0)
offsetXY HeadingS = (0,1)
offsetXY HeadingW = (-1,0)

terrains :: Map (Int,Int) (Maybe [Heading]) -> [Map (Int,Int) (Maybe [Heading])]
terrains terrain0 = iterate nextTerrain terrain0
  where
    nextTerrain :: Map (Int,Int) (Maybe [Heading]) -> Map (Int,Int) (Maybe [Heading])
    nextTerrain terrain = foldr (Map.unionWith (\(Just a) (Just b) -> Just $ a ++ b))
                                Map.empty $ concat
                                [ case headings of
                                    Nothing -> [Map.singleton point Nothing]
                                    Just headings -> map (\heading -> Map.singleton (offsetStep heading point) (Just [heading])) $ headings
                                | (point, headings) <- Map.toList terrain
                                ]
    terrainWidth = maximum . map fst . Map.keys $ terrain0
    terrainHeight = maximum . map snd . Map.keys $ terrain0

    offsetStep heading (x,y)
      | x == 1 && heading == HeadingW = (terrainWidth - 1, y)
      | y == 1 && heading == HeadingN = (x, terrainHeight - 1)
      | x == terrainWidth - 1 && heading == HeadingE = (1, y)
      | y == terrainHeight - 1 && heading == HeadingS = (x, 1)
      | otherwise = let (x',y') = offsetXY heading
                     in (x + x', y + y')

printTerrain terrain = putStrLn $ unlines [ [ case Map.lookup (x,y) terrain of
                                                Just (Nothing) -> '#'
                                                Just (Just [HeadingN]) -> '^'
                                                Just (Just [HeadingE]) -> '>'
                                                Just (Just [HeadingS]) -> 'v'
                                                Just (Just [HeadingW]) -> '<'
                                                Just (Just xs) -> if length xs > 9 then '!' else head . show $ length xs
                                                Nothing -> '.'
                                            | x <- [minX..maxX] ]
                                          | y <- [minY..maxY]]
  where
    minX = minimum . map fst $ Map.keys terrain
    maxX = maximum . map fst $ Map.keys terrain
    minY = minimum . map snd $ Map.keys terrain
    maxY = maximum . map snd $ Map.keys terrain

testTerrain :: Char -> Parsed
testTerrain heading = parseInput $ unlines ["#.###","#...#","#." ++ [heading] ++ ".#","#...#","###.#"]

runTest terrain = forM_ (take 10 $ terrains terrain) printTerrain

buildGraph terrain0 stormCycle = Map.unions (go 0 (tail $ terrains terrain0))
  where
    go n (terrain:terrains)
      | n == stormCycle = []
      | otherwise = [ Map.singleton ((x,y),n) 
                                    [(1, ((x',y'),(n+1) `mod` stormCycle))
                                    | (xd,yd) <- (0,0):(offsetXY <$> allHeadings)
                                    , let (x', y') = (x + xd, y + yd)
                                    , valid (x',y')
                                    , case Map.lookup (x', y') terrain of
                                        Nothing -> True
                                        Just (Just []) -> True
                                        _ -> False
                                    ]
                    | x <- [minX..maxX], y <- [minY..maxY]
                    , valid (x,y)
                    ] ++ go (n+1) terrains
    minX = minimum . map fst $ Map.keys terrain0
    maxX = maximum . map fst $ Map.keys terrain0
    minY = minimum . map snd $ Map.keys terrain0
    maxY = maximum . map snd $ Map.keys terrain0

    valid (x,y)
      | y == minY && x /= minX + 1 = False
      | y == maxY && x /= maxX - 1 = False
      | x < minX || x > maxX || y < minY || y > maxY = False
      | otherwise = True

loopLength = go Set.empty 0
  where 
    go set n (x:xs) | Set.member x set = n
                    | otherwise = go (Set.insert x set) (n+1) xs

part1 :: Parsed -> IO ()
part1 terrain = do runTest $ testTerrain '<'
                   runTest $ testTerrain '>'
                   runTest $ testTerrain '^'
                   runTest $ testTerrain 'v'
                   let mapWidth = maximum . map fst . Map.keys $ terrain
                   let mapHeight = maximum . map snd . Map.keys $ terrain
                   let stormCycle = loopLength $ terrains terrain
                   let graph = buildGraph terrain stormCycle
                   putStrLn $ show $ Map.size graph
                   putStrLn $ show $ dijkstraAny graph ((1,0),0) (\(p,_) -> p == (mapWidth - 1, mapHeight))
                   --putStrLn $ show $ bfs terrain

part2 :: Parsed -> IO ()
part2 terrain = do let mapWidth = maximum . map fst . Map.keys $ terrain
                   let mapHeight = maximum . map snd . Map.keys $ terrain
                   let stormCycle = loopLength $ terrains terrain
                   let graph = buildGraph terrain stormCycle
                   putStrLn $ show $ Map.size graph
                   let timeOut = dijkstraAny graph ((1,0),0) (\(p,_) -> p == (mapWidth - 1, mapHeight))
                   putStrLn $ show timeOut
                   let timeBack = dijkstraAny graph ((mapWidth - 1,mapHeight),timeOut `mod` stormCycle) (\(p,_) -> p == (1, 0))
                   putStrLn $ show timeBack
                   let timeOutAgain = dijkstraAny graph ((1,0),(timeOut+timeBack) `mod` stormCycle) (\(p,_) -> p == (mapWidth - 1, mapHeight))
                   putStrLn $ show timeOutAgain
                   putStrLn $ show $ timeOut + timeBack + timeOutAgain


day24 part args = do
  let filename = case args of
                   [] -> "inputs/day24"
                   [f] -> f
  inputs <- parseInput <$> readFile filename
  case part of
    Part1 -> part1 inputs
    Part2 -> part2 inputs
