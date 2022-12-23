{-# LANGUAGE TupleSections, ScopedTypeVariables  #-}
module Day12 (day12) where
import Part (Part (Part1, Part2))

import Data.List (sort, foldl')
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe (catMaybes, fromMaybe, mapMaybe)

import Graph (invertGraph, dijkstra, dijkstraAny)

type Parsed = Map (Int,Int) Char

parseInput :: String -> Parsed
parseInput input = Map.fromList [ ((x,y),height)
                                | (x,cols) <- withCoords input
                                , (y,height) <- cols
                                ]
  where withCoords :: String -> [(Int,[(Int,Char)])]
        withCoords = zip [0..] . map (zip [0..]) . lines

neighbours :: (Int, Int) -> Map (Int, Int) v -> [((Int, Int), v)]
neighbours (x, y) map = catMaybes [ adj (-1) 0
                                  , adj 0 1
                                  , adj 1 0
                                  , adj 0 (-1)
                                  ]
  where adj xd yd = let p = (x + xd, y + yd)
                     in (p,) <$> Map.lookup p map

graphFromInput :: Map (Int,Int) Char -> ((Int,Int), (Int,Int), Map (Int,Int) [(Int, (Int,Int))])
graphFromInput inputMap = (startPoint
                          ,endPoint
                          ,Map.fromList [ (k,[ (1, neighbourCoord)
                                            | (neighbourCoord, neighbourHeight) <- neighbours k heightMap
                                            , neighbourHeight <= succ myHeight
                                            ])
                                       | (k,myHeight) <- Map.toList heightMap
                                       ]
                          )
  where
    startPoint = head [k | (k,v) <- Map.toList inputMap, v == 'S']
    endPoint   = head [k | (k,v) <- Map.toList inputMap, v == 'E']
    heightMap  = Map.insert startPoint 'a' $ Map.insert endPoint 'z' $ inputMap





part1 :: Parsed -> IO ()
part1 inputMap = do let (start, end, graph) = graphFromInput inputMap
                    putStrLn $ show $ dijkstra graph start end

part2 :: Parsed -> IO ()
part2 inputMap = do let (start, end, graph) = graphFromInput inputMap
                    let potentialStartPoints = Set.fromList [k | (k, v) <- Map.toList inputMap, v == 'a' || v == 'S']
                    putStrLn $ show $ dijkstraAny (invertGraph graph) end (`Set.member` potentialStartPoints)

day12 part args = do
  let filename = case args of
                   [] -> "inputs/day12"
                   [f] -> f
  inputs <- parseInput <$> readFile filename
  case part of
    Part1 -> part1 inputs
    Part2 -> part2 inputs
