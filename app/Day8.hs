module Day8 (day8) where
import Part (Part (Part1, Part2))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Parsed = Map (Int,Int) Int 

parseInput :: String -> Parsed
parseInput input = Map.fromList [ ((x,y),height)
                                | (x,cols) <- withCoords input
                                , (y,height) <- cols
                                ]
  where withCoords :: String -> [(Int,[(Int,Int)])]
        withCoords = zip [0..] . map (zip [0..] . map (read . pure)) . lines

part1 :: Parsed -> IO ()
part1 treemap = let minx = minimum (fst <$> Map.keys treemap)
                    maxx = maximum (fst <$> Map.keys treemap)
                    miny = minimum (snd <$> Map.keys treemap)
                    maxy = maximum (snd <$> Map.keys treemap)
                    hidden = [ (x,y)
                              | x <- [minx+1..maxx-1]
                              , y <- [miny+1..maxy-1]
                              , let height = (Map.!) treemap (x,y)
                              , let neighbourSets = [[(x', y) | x' <- [minx..x-1]]
                                                    ,[(x', y) | x' <- [x+1..maxx]]
                                                    ,[(x, y') | y' <- [miny..y-1]]
                                                    ,[(x, y') | y' <- [y+1..maxy]]]
                              , all (== True)
                                    [ any (>= height) . fmap ((Map.!) treemap) $ neighbourSet
                                    | neighbourSet <- neighbourSets
                                    ]
                              ]
                 in putStrLn . show . ((Map.size treemap) -) . length $ hidden

part2 :: Parsed -> IO ()
part2 treemap = let minx = minimum (fst <$> Map.keys treemap)
                    maxx = maximum (fst <$> Map.keys treemap)
                    miny = minimum (snd <$> Map.keys treemap)
                    maxy = maximum (snd <$> Map.keys treemap)
                    scores = [ (scenicScore height (-1) 0 (x-1) y) *
                               (scenicScore height 1 0 (x+1) y) *
                               (scenicScore height 0 (-1) x (y-1)) *
                               (scenicScore height 0 (1) x (y+1))
                              | x <- [minx..maxx]
                              , y <- [miny..maxy]
                              , let height = (Map.!) treemap (x,y)
                              ]
                    scenicScore height xOff yOff x y
                      | x < minx || x > maxx || y < miny || y > maxy = 0
                      | (Map.!) treemap (x, y) >= height             = 1
                      | otherwise                                    = 1 + scenicScore height xOff yOff (x+xOff) (y+yOff)
                 in putStrLn . show . maximum $ scores 

day8 part args = do let filename = case args of
                                      [] -> "inputs/day8"
                                      [f] -> f
                    inputs <- parseInput <$> readFile filename
                    case part of
                      Part1 -> part1 inputs
                      Part2 -> part2 inputs
