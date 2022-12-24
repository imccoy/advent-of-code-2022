module Day18 (day18) where

import Control.Monad (forM_)
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

import Part (Part (Part1, Part2))

type Parsed = Set (Int,Int,Int)

parseInput :: String -> Parsed
parseInput = Set.fromList . map (\[a,b,c] -> (read a, read b, read c)) . map (splitOn ",") . lines

data Face = FTop | FBottom | FLeft | FRight | FFront | FBack
  deriving (Enum, Ord, Eq, Show)

look (x,y,z) FTop = (x,y+1,z)
look (x,y,z) FBottom = (x,y-1,z)
look (x,y,z) FLeft = (x-1,y,z)
look (x,y,z) FRight = (x+1,y,z)
look (x,y,z) FFront = (x,y,z-1)
look (x,y,z) FBack = (x,y,z+1)

opposite FTop = FBottom
opposite FBottom = FTop
opposite FLeft = FRight
opposite FRight = FLeft
opposite FFront = FBack
opposite FBack = FFront

data Axis = AxisX | AxisY | AxisZ
  deriving (Enum, Ord, Eq, Show)

faceAxis FTop = AxisY
faceAxis FBottom = AxisY
faceAxis FRight = AxisX
faceAxis FLeft = AxisX
faceAxis FFront = AxisZ
faceAxis FBack = AxisZ

axisFaces AxisY = [FTop, FBottom]
axisFaces AxisX = [FLeft, FRight]
axisFaces AxisZ = [FFront, FBack]

orthoN :: [Face] -> [Face]
orthoN face = [ face
              | axis <- [AxisX, AxisY, AxisZ]
              , not (elem axis $ faceAxis <$> face)
              , face <- axisFaces axis
              ]

orthogonal f = orthoN [f]

--  neighbourPaths FTop = [([FLeft],FTop)
--                        ,([FRight],FTop)
--                        ,([FFront],FTop)
--                        ,([FBack],FTop)
--                        ,([FLeft,FTop],FRight)
--                        ,([FRight,FTop],FLeft)
--                        ,([FFront,FTop],FBack)
--                        ,([FFront,FTop],FBack)
--                        ]

neighbourFaces :: ((Int,Int,Int),Face) -> Set ((Int,Int,Int),Face) -> Set (Int,Int,Int) -> [((Int,Int,Int),Face)]
neighbourFaces (node, dir) allFaces allNodes = filter (`Set.member` allFaces) $
                                                      concat [ (if not (Set.member (look (look node o) dir) allNodes) then [(node, o)] else [])
                                                               ++ 
                                                               [(look node o, dir),
                                                                (look (look node o) dir, opposite o)]
                                                             | o <- orthogonal dir]

openFaces cubes = concat . fmap faces1 . Set.toList $ cubes
  where faces1 cube = [(cube, face) | face <- [(FTop)..(FBack)], not (Set.member (look cube face) cubes)]

part1 :: Parsed -> IO ()
part1 cubes = putStrLn . show . length . openFaces $ cubes

categorize :: [[((Int,Int,Int),Face)]] -> Set ((Int,Int,Int),Face) -> Set ((Int,Int,Int),Face) -> Set (Int,Int,Int) -> [((Int,Int,Int),Face)] -> [[((Int,Int,Int),Face)]]
categorize categories visited allFaces allNodes [] = categories
categorize categories visited allFaces allNodes (f:faces)
 | Set.member f visited = categorize categories visited allFaces allNodes faces 
 | otherwise = let (category, visited') = exploreCategory [f] Set.empty
                in categorize (category:categories) (Set.union visited visited') allFaces allNodes faces
 where 
   exploreCategory [] inCategory = (Set.toList inCategory, inCategory)
   exploreCategory (face:faces) inCategory
     | Set.member face inCategory = exploreCategory faces inCategory
     | otherwise = let ns = neighbourFaces face allFaces allNodes 
                    in exploreCategory (faces ++ ns) (Set.insert face inCategory)

part2 :: Parsed -> IO ()
part2 cubes = do let categorizedFaces = categorize [] Set.empty (Set.fromList $ openFaces cubes) cubes (openFaces cubes)
                  in do forM_ categorizedFaces $ \category -> putStrLn $ show $ length category

day18 part args = do
  let filename = case args of
                   [] -> "inputs/day18"
                   [f] -> f
  inputs <- parseInput <$> readFile filename
  case part of
    Part1 -> part1 inputs
    Part2 -> part2 inputs
