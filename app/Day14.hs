{-# LANGUAGE TupleSections,ScopedTypeVariables,FlexibleContexts #-}
module Day14 (day14) where

import Control.Lens ((&),(^.),(<&>),(%~),view)
import Control.Zipper
import Data.List (groupBy, sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

import Part (Part (Part1, Part2))

type Parsed = [[((Int,Int),(Int,Int))]]

parseInput :: String -> Parsed
parseInput = fmap parseLine . lines
  where parseLine = startEnds . map (\[x,y] -> (read x,read y)) . map (splitOn ",")  . splitOn " -> "
        startEnds ps = zip ps (drop 1 ps)

toStripes :: [[((Int, Int), (Int, Int))]] -> [[(Int, Int)]]
toStripes = withEmpties 0 . map (\(xs, y1y2s) -> (head xs, simplifyStripe . sort $ y1y2s)) . map unzip . groupBy (\a b -> fst a == fst b) . sort . concat . map stripe1 . concat
  where stripe1 ((x1,y1),(x2,y2)) | x1 == x2  = [(x1, (min y1 y2,max y1 y2))]
                                  | otherwise = (,(y1,y1)) <$> [min x1 x2..max x1 x2]
        simplifyStripe ((y1a,y2a):(y1b,y2b):stripe) | y1b <= y2a + 1 = simplifyStripe $ (y1a,y2b):stripe
                                                    | otherwise = (y1a,y2a):simplifyStripe ((y1b,y2b):stripe)
        simplifyStripe [segment] = [segment]

        withEmpties n ((x,segments):stripe) | n == x = segments:(withEmpties (n+1) stripe)
                                            | otherwise = []:(withEmpties (n+1) ((x,segments):stripe))


data InspectStripe = InspectStripeFull | InspectStripeFreeUntil Int | InspectStripeFreeFall

inspectStripe [] y = InspectStripeFreeFall
inspectStripe ((y1a,y2a):segments) y | y1a <= y && y <= y2a = InspectStripeFull
                                     | y < y1a              = InspectStripeFreeUntil $ y1a - 1
                                     | otherwise            = case segments of
                                                                ((y1b,_):_) -> if y < y1b
                                                                                 then InspectStripeFreeUntil $ y1b - 1
                                                                                 else inspectStripe segments y
                                                                [] -> InspectStripeFreeFall

addToStripe y ((y1,y2):segments) | y == y1 - 1 = (y,y2):segments
                                 | otherwise   = (y1,y2):addToStripe y segments

snowToFill :: forall h i. (Zipping h [(Int,Int)]) => Zipper h i [(Int, Int)] -> Int
snowToFill stripesZ = countSnow startLocStripes
  where countSnow :: Zipper h i [(Int, Int)] -> Int
        countSnow stripes = landFrom0 (fromJust $ restoreTape startLoc $ rezip stripes)
        landFrom0 :: Zipper h i [(Int, Int)] -> Int
        landFrom0 stripes = case inspectStripe (stripes & view focus) 0 of
                              InspectStripeFreeFall -> 0
                              InspectStripeFreeUntil y -> landFrom stripes y
        landFrom :: Zipper h i [(Int, Int)] -> Int -> Int
        landFrom stripes h = case inspectStripe (fromJust (stripes & leftward <&> view focus)) (h+1) of
                               InspectStripeFreeFall -> 0
                               InspectStripeFreeUntil y -> landFrom (fromJust (stripes & leftward)) y
                               InspectStripeFull -> case inspectStripe (fromJust (stripes & rightward <&> view focus)) (h+1) of
                                 InspectStripeFreeFall -> 0
                                 InspectStripeFreeUntil y -> landFrom (fromJust (stripes & rightward)) y
                                 InspectStripeFull -> 1 + countSnow (stripes & focus %~ addToStripe h)
        startLoc = saveTape startLocStripes
        startLocStripes = fromJust (stripesZ & leftmost & jerks rightward 500)
         

part1 :: Parsed -> IO ()
part1 input = do
  let stripes = toStripes input
  let stripesZ = zipper stripes & fromWithin traverse
  putStrLn $ show $ snowToFill stripesZ
  

part2 :: Parsed -> IO ()
part2 _ = putStrLn "part2"

day14 part args = do
  let filename = case args of
                   [] -> "inputs/day14"
                   [f] -> f
  inputs <- parseInput <$> readFile filename
  case part of
    Part1 -> part1 inputs
    Part2 -> part2 inputs
