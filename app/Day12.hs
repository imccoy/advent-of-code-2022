{-# LANGUAGE TupleSections, ScopedTypeVariables  #-}
module Day12 (day12) where
import Part (Part (Part1, Part2))

import Data.List (sort)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe (catMaybes, fromMaybe, mapMaybe)

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


dijkstra :: forall k. (Eq k, Ord k) => Map k [(Int, k)] -> k -> k -> Int
dijkstra graph start end =
  let initialCosts = const maxBound <$> graph
      initialHeap = Map.unionsWith (<>) $ (\x -> Map.singleton maxBound [x]) <$> Map.keys graph
      pullLowCostNode :: Map Int [k] -> (k, Map Int [k])
      pullLowCostNode map = let ((minCost, (p:restPoints)), rest) = Map.deleteFindMin map
                             in (p, case restPoints of
                                      [] -> rest
                                      _ -> Map.insert minCost restPoints rest)
      updateCostsHeap :: (Map k Int, Map Int [k]) -> k -> Int -> (Map k Int, Map Int [k])
      updateCostsHeap (costs, heap) point cost = ( Map.insert point cost costs
                                                 , Map.alter ((Just . (point:)) . fromMaybe []) cost heap
                                                     -- in theory we shoud delete the point from the old cost at `heap ! (costs ! point)`
                                                     -- at this point, but it's not worth it, and it's not actually necessary
                                                 )
      next (costs, heap) visited = let (next, heap') = pullLowCostNode heap
                                    in go next (costs, heap') visited

      go :: k -> (Map k Int, Map Int [k]) -> Set k -> Int
      go node (costs, heap) visited
        | node == end = costs ! end
        | Set.member node visited = next (costs, heap) visited
        | otherwise = let costToHere = costs ! node
                          (costs', heap') = foldr (\(c, n) ch -> updateCostsHeap ch n c) (costs, heap) .
                                              mapMaybe (\(c, n) -> if costToHere + c < costs ! n then Just (costToHere + c, n) else Nothing) $
                                              graph ! node
                       in next (costs', heap') (Set.insert node visited)
   in go start (updateCostsHeap (initialCosts,  initialHeap) start 0) Set.empty



part1 :: Parsed -> IO ()
part1 inputMap = do let (start, end, graph) = graphFromInput inputMap
                    putStrLn $ show $ dijkstra graph start end

part2 :: Parsed -> IO ()
part2 inputMap = do let (start, end, graph) = graphFromInput inputMap
                    let potentialStartPoints = [k | (k, v) <- Map.toList inputMap, v == 'a' || v == 'S']
                    putStrLn $ show $ sort $ fmap (\start -> dijkstra graph start end) potentialStartPoints

day12 part args = do
  let filename = case args of
                   [] -> "inputs/day12"
                   [f] -> f
  inputs <- parseInput <$> readFile filename
  case part of
    Part1 -> part1 inputs
    Part2 -> part2 inputs
