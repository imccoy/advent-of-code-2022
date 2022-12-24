module Day16 (day16) where

import Control.Monad (forM_)
import Data.List (sort)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Text.Parsec

import Graph
import Part (Part (Part1, Part2))

newtype FlowRate = FlowRate Int
  deriving (Show, Eq, Ord)

type Parsed = [(String, (FlowRate, [String]))]

parseReportLine = do string "Valve "
                     label <- count 2 upper
                     string " has flow rate="
                     flowRate <- many1 digit
                     (try (string "; tunnels lead to valves ") <|> string "; tunnel leads to valve ")
                     tunnels <- sepBy (count 2 upper) (string ", ")
                     pure (label, (FlowRate $ read flowRate, tunnels))

costsGraph :: Parsed -> [(String, Map String Int)]
costsGraph graph = let onesGraph = Map.fromList [(label, [(1, tunnel) | tunnel <- tunnels]) | (label, (_, tunnels)) <- graph]
                       tapNodes = [label | (label, (flowRate, _)) <- graph, flowRate /= FlowRate 0]
                    in [ (label, dijkstraAll onesGraph label (\_ _ _ -> Nothing) (Map.filterWithKey (\l' _ -> elem l' tapNodes && l' /= label)))
                       | label <- "AA":tapNodes
                       ]


parseInput :: String -> Parsed
parseInput = either (error . show) id . runParser (endBy parseReportLine $ string "\n") () "none"

explore :: Parsed -> [(String, Map String Int)] -> [String] -> Int -> [Int] -> [String] -> [(Int, [String])]
explore graph costsGraph = go 
 where go labels volume timesRemaining alreadyOn
         = case [ finalVolume
                | (nextLabels, nextVolume, nextTimesRemaining) <-
                    foldr (\(l, t) options ->
                            concat [ [ nextVolume `seq` nextTimeRemaining `seq` (nextRoom:ls, nextVolume, nextTimeRemaining:ts)
                                     | (ls, v, ts) <- options
                                     , not (elem nextRoom ls)
                                     , let nextVolume = v + (t - distance - 1) * flowRate, let nextTimeRemaining = t - (distance + 1)
                                     ]
                                   | (nextRoom, distance) <- Map.assocs $ fromJust $ lookup l costsGraph
                                   , not (elem nextRoom alreadyOn) && not (elem nextRoom labels)
                                   , distance + 1 < t
                                   , let (FlowRate flowRate, _) = fromJust $ lookup nextRoom graph
                                   ]
                          )
                          [([], volume, [])]
                          (zip labels timesRemaining)
                , finalVolume <- go nextLabels nextVolume nextTimesRemaining (labels ++ alreadyOn)
                ] of
             [] -> [(volume, alreadyOn)]
             finalVolumes -> finalVolumes

part1 :: Parsed -> IO ()
part1 graph = do
  putStrLn $ show $ costsGraph graph
  forM_ (sort $ explore graph (costsGraph graph) ["AA"] 0 [30] []) $ \volume -> putStrLn $ show volume

part2 :: Parsed -> IO ()
part2 graph = do
  putStrLn $ show $ maximum $ explore graph (costsGraph graph) ["AA", "AA"] 0 [26,26] []

day16 part args = do
  let filename = case args of
                   [] -> "inputs/day16"
                   [f] -> f
  inputs <- parseInput <$> readFile filename
  case part of
    Part1 -> part1 inputs
    Part2 -> part2 inputs
