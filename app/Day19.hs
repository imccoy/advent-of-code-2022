module Day19 (day19) where

import Control.Monad (forM_, forM)
import Data.List (maximumBy, nub, sort, sortBy)
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Debug.Trace
import Text.Parsec

import Part (Part (Part1, Part2))

type Parsed = [Blueprint]

data Blueprint = Blueprint { blueprintNumber :: Int
                           , oreOre :: Int
                           , clayOre :: Int
                           , obsidianOre :: Int
                           , obsidianClay :: Int
                           , geodeOre :: Int
                           , geodeObsidian :: Int
                           }
  deriving (Show)

parseBlueprintLine = do string "Blueprint "
                        blueprintNumber <- read <$> many1 digit
                        string ": "
                        string "Each ore robot costs "
                        oreOre <- read <$> many1 digit
                        string " ore"
                        string ". "
                        string "Each clay robot costs "
                        clayOre <- read <$> many1 digit
                        string " ore"
                        string ". "
                        string "Each obsidian robot costs "
                        obsidianOre <- read <$> many1 digit
                        string " ore"
                        string " and "
                        obsidianClay <- read <$> many1 digit
                        string " clay"
                        string ". "
                        string "Each geode robot costs "
                        geodeOre <- read <$> many1 digit
                        string " ore"
                        string " and "
                        geodeObsidian <- read <$> many1 digit
                        string " obsidian"
                        string "."
                        pure $ Blueprint blueprintNumber oreOre clayOre obsidianOre obsidianClay geodeOre geodeObsidian

parseInput :: String -> Parsed
parseInput = either (error . show) id . runParser (endBy parseBlueprintLine $ string "\n") () "none"

data Robot = Ore | Clay | Obsidian | Geode
  deriving (Show, Eq, Ord)

data Stocks = Stocks { oreStock :: Int, clayStock :: Int, obsidianStock :: Int, geodeStock :: Int
                     , oreRobots :: Int, clayRobots :: Int, obsidianRobots :: Int, geodeRobots :: Int }
  deriving (Show, Eq, Ord)

--produced byTime buildEndTimes = sum . map (byTime -) . (filter (< byTime))
--oreProduced byTime = produced byTime . (24:)
--oreAvailable blueprint byTime oreBuildEndTimes clayBuildEndTimes obsidianBuildEndTimes geodeBuildEndTimes
--  = produced byTime oreBuildEndTimes - oreConsumed blueprint (length oreBuildEndTimes) (length clayBuildEndTimes) (length obsidianBuildEndTimes) (length geodeBuildEndTimes)
--oreConsumed blueprint ore clay obsidian geode = ore * oreOre blueprint + clay * clayOre blueprint + obsidian * obisidianOre blueprint + geode * geodeOre blueprint
--clayConsumed blueprint obsidian = obsidian * obsidianClay blueprint
--obsidianConsumed blueprint geode = geode * geodeObsidian blueprint

maxOreProduced blueprint = go 24 0 0 1
  where go 0 oreAvailable _ _ = oreAvailable
        go timeRemaining oreAvailable oreProduced oreRobots | oreAvailable >= oreOre blueprint = go (timeRemaining - 1) (oreAvailable - oreOre blueprint + oreRobots) (oreProduced + oreRobots) (oreRobots + 1)
                                                            | otherwise                        = go (timeRemaining - 1) (oreAvailable + oreRobots) (oreProduced + oreRobots) oreRobots

compareByFfs [] _ _ = EQ
compareByFfs (f:fs) a b = case compare (f b) (f a) of
                            EQ -> compareByFfs fs a b
                            o -> o

optimise2 timeAvailable blueprint = go timeAvailable [Stocks 0 0 0 0 1 0 0 0]
  where
    go :: Int -> [Stocks] -> Int
    go 0 bests = maximum $ geodeStock <$> bests
    go timeRemaining bests = let candidates = (catMaybes [tryRobot robot best | best <- bests, robot <- [Ore, Clay, Obsidian, Geode]]) ++ map mine bests
                                 bests' = bestsOf candidates
                              in go (timeRemaining - 1) bests'
      where tryRobot robot stocks = let resourcesNeeded = resourcesFor robot
                                     in if haveResources resourcesNeeded stocks
                                          then Just $ addRobot robot $ mine $ takeResources resourcesNeeded stocks
                                          else Nothing
            bestsOf :: [Stocks] -> [Stocks]
            bestsOf options = nub [ option
                                  | f  <- [oreStock,clayStock,obsidianStock,geodeStock,oreRobots,clayRobots,obsidianRobots,geodeRobots]
                                  , let bests = filter (/= 0) . take 40 . sortBy (\a b -> compare b a) $ (f <$> options)
                                  , option <- take 400 . sortBy (compareByFfs [geodeRobots, geodeStock, obsidianRobots, obsidianStock, clayRobots, clayStock, oreRobots, oreStock]) . filter (\option -> elem (f option) bests) $ options
                                  ]
    resourcesFor robot = case robot of
                           Ore ->      (oreOre blueprint,      0,                      0)
                           Clay ->     (clayOre blueprint,     0,                      0)
                           Obsidian -> (obsidianOre blueprint, obsidianClay blueprint, 0)
                           Geode ->    (geodeOre blueprint,    0,                      geodeObsidian blueprint)

    haveResources (needOre, needClay, needObsidian) stocks = oreStock stocks >= needOre && clayStock stocks >= needClay && obsidianStock stocks >= needObsidian
                                            
    mineN n stocks = stocks { oreStock = (oreStock stocks) + (oreRobots stocks) * n
                            , clayStock = (clayStock stocks) + (clayRobots stocks) * n
                            , obsidianStock = (obsidianStock stocks) + (obsidianRobots stocks) * n
                            , geodeStock = (geodeStock stocks) + (geodeRobots stocks) * n
                            }
    mine = mineN 1
    takeResources (takeOre, takeClay, takeObsidian) stocks = stocks { oreStock = (oreStock stocks) - takeOre
                                                                    , clayStock = (clayStock stocks) - takeClay
                                                                    , obsidianStock = (obsidianStock stocks) - takeObsidian
                                                                    }
 
    addRobot Ore stocks = stocks { oreRobots = (oreRobots stocks) + 1 }
    addRobot Clay stocks = stocks { clayRobots = (clayRobots stocks) + 1 }
    addRobot Obsidian stocks = stocks { obsidianRobots = (obsidianRobots stocks) + 1 }
    addRobot Geode stocks = stocks { geodeRobots = (geodeRobots stocks) + 1 }

 
optimise blueprint = go 24 (Stocks 0 0 0 0 1 0 0 0) []
  where
    go :: Int -> Stocks -> [((Int, Int, Int), Robot)] -> [(Int, [((Int, Int, Int), Robot)])]
    go 0 stocks path = [(geodeStock stocks, path)]
    go 1 stocks path = [(geodeStock $ mine stocks, path)]
    go timeRemaining stocks path = tryRobots [Geode, Obsidian, Clay, Ore]
      where tryRobots (nextRobot:otherRobots) = (case timeToMine stocks nextRobot of
                                                   Just t -> if t < timeRemaining
                                                                then let stocksAfterMining = mineN t stocks
                                                                         timeAfterMining = timeRemaining - t
                                                                         stocksAfterTakingResources = takeResources (resourcesFor nextRobot) stocksAfterMining
                                                                      in if timeAfterMining - 1 == 0
                                                                           then []
                                                                           else go (timeAfterMining - 1) (addRobot nextRobot $ mine stocksAfterTakingResources) (((timeRemaining, timeAfterMining, timeAfterMining - 1),nextRobot):path)
                                                                else [(geodeStock $ mineN timeRemaining stocks, path)]
                                                   Nothing -> []
                                                ) ++ tryRobots otherRobots
            tryRobots [] = []
    resourcesFor robot = case robot of
                           Ore ->      (oreOre blueprint,      0,                      0)
                           Clay ->     (clayOre blueprint,     0,                      0)
                           Obsidian -> (obsidianOre blueprint, obsidianClay blueprint, 0)
                           Geode ->    (geodeOre blueprint,    0,                      geodeObsidian blueprint)

    timeToMine stocks robot = let (needOre, needClay, needObsidian) = resourcesFor robot
                                  timeRequired rateOfIncrease amountRequired currentStock
                                    | amountRequired <= currentStock = Just 0
                                    | rateOfIncrease == 0            = Nothing
                                    | otherwise = Just $ (amountRequired - currentStock + rateOfIncrease - 1) `div` rateOfIncrease
                               in maximum <$> sequence [ timeRequired (oreRobots stocks) needOre (oreStock stocks)
                                                       , timeRequired (clayRobots stocks) needClay (clayStock stocks)
                                                       , timeRequired (obsidianRobots stocks) needObsidian (obsidianStock stocks)
                                                       ]
                                            
    mineN n stocks = stocks { oreStock = (oreStock stocks) + (oreRobots stocks) * n
                            , clayStock = (clayStock stocks) + (clayRobots stocks) * n
                            , obsidianStock = (obsidianStock stocks) + (obsidianRobots stocks) * n
                            , geodeStock = (geodeStock stocks) + (geodeRobots stocks) * n
                            }
    mine = mineN 1
    takeResources (takeOre, takeClay, takeObsidian) stocks = stocks { oreStock = (oreStock stocks) - takeOre
                                                                    , clayStock = (clayStock stocks) - takeClay
                                                                    , obsidianStock = (obsidianStock stocks) - takeObsidian
                                                                    }
 
    addRobot Ore stocks = stocks { oreRobots = (oreRobots stocks) + 1 }
    addRobot Clay stocks = stocks { clayRobots = (clayRobots stocks) + 1 }
    addRobot Obsidian stocks = stocks { obsidianRobots = (obsidianRobots stocks) + 1 }
    addRobot Geode stocks = stocks { geodeRobots = (geodeRobots stocks) + 1 }

part1 :: Parsed -> IO ()
part1 blueprints = do optimised <- forM blueprints $ \blueprint -> do
                                     putStrLn . show $ blueprint
                                     let r = optimise2 24 blueprint
                                     putStrLn . show $ r
                                     pure (blueprint, r)
                      putStrLn . show . sum . map (\(blueprint, r) -> blueprintNumber blueprint * r) $ optimised

part2 :: Parsed -> IO ()
part2 blueprints = do optimised <- forM (take 3 blueprints) $ \blueprint -> do
                                     putStrLn . show $ blueprint
                                     let r = optimise2 32 blueprint
                                     putStrLn . show $ r
                                     pure r
                      putStrLn . show . product $ optimised


day19 part args = do
  let filename = case args of
                   [] -> "inputs/day19"
                   [f] -> f
  inputs <- parseInput <$> readFile filename
  case part of
    Part1 -> part1 inputs
    Part2 -> part2 inputs
