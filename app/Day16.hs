module Day16 (day16) where

import Control.Monad (forM_)
import Data.List (sortBy)
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

costsGraph graph = let onesGraph = Map.fromList [(label, [(1, tunnel) | tunnel <- tunnels]) | (label, (_, tunnels)) <- graph]
                       tapNodes = [label | (label, (flowRate, _)) <- graph, flowRate /= FlowRate 0]
                    in [ (label, dijkstraAll onesGraph label (\_ _ _ -> Nothing) (Map.filterWithKey (\l' _ -> elem l' tapNodes && l' /= label)))
                       | label <- "AA":tapNodes
                       ]


parseInput :: String -> Parsed
parseInput = either (error . show) id . runParser (endBy parseReportLine $ string "\n") () "none"

explore graph costsGraph = go 
 where go label volume timeRemaining alreadyOn
        | otherwise
         = case [ ((timeRemaining - distance - 1) * flowRate, distance + 1, nextRoom)
                | (nextRoom, distance) <- Map.assocs $ fromJust $ lookup label costsGraph
                , not (elem nextRoom alreadyOn)
                , distance + 1 < timeRemaining
                , let (FlowRate flowRate, _) = fromJust $ lookup nextRoom graph
                ] of
             [] -> [(volume, alreadyOn)]
             options -> [ finalVolume
                        | (v', t', l') <- sortBy (\(v1,t1,_) (v2,t2,_) -> case compare v2 v1 of
                                                                            EQ -> compare t1 t2
                                                                            c -> c
                                                 ) options
                        , let nextVolume = volume + v', let nextTimeRemaining = timeRemaining - t'
                        , finalVolume <- nextVolume `seq` nextTimeRemaining `seq` go l' nextVolume nextTimeRemaining (label:alreadyOn)
                        ]

part1 :: Parsed -> IO ()
part1 graph = do
  putStrLn $ show $ costsGraph graph
  forM_ (explore graph (costsGraph graph) "AA" 0 30 []) $ \volume -> putStrLn $ show volume

part2 :: Parsed -> IO ()
part2 _ = putStrLn "part2"

day16 part args = do
  let filename = case args of
                   [] -> "inputs/day16"
                   [f] -> f
  inputs <- parseInput <$> readFile filename
  case part of
    Part1 -> part1 inputs
    Part2 -> part2 inputs
