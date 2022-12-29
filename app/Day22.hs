{-# LANGUAGE TupleSections #-}
module Day22 (day22) where

import Control.Monad (forM_)
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, maybeToList)
import Debug.Trace

import Part (Part (Part1, Part2))

type Parsed = (Map (Int,Int) MapCell, [Step])


data MapCell = MapCellSpace | MapCellWall
  deriving (Show, Eq)

data Turn = TurnL | TurnR
  deriving (Show)

data Step = StepWalk Int | StepTurn Turn
  deriving (Show)

parseInput :: String -> Parsed
parseInput input = let [mapLines, [moves]] = splitOn [""] $ lines input
                    in (parseMap (zip [0..] . map (zip [0..]) $ mapLines), parseMoves moves)
  where parseMap mapWithCoords = Map.fromList [((x, y), c)
                                              | (y, cols) <- mapWithCoords
                                              , (x, char) <- cols
                                              , c <- maybeToList $ parseMapCell char
                                              ]
        parseMapCell '.' = Just MapCellSpace
        parseMapCell '#' = Just MapCellWall
        parseMapCell ' ' = Nothing
        parseMoves "" = []
        parseMoves "\n" = []
        parseMoves ('L':s) = (StepTurn TurnL):(parseMoves s)
        parseMoves ('R':s) = (StepTurn TurnR):(parseMoves s)
        parseMoves s = let (d,s') = parseDigit 0 s in (StepWalk d):(parseMoves s')
        parseDigit n (c:cs) | isDigit c = parseDigit (n * 10 + read [c]) cs
                            | otherwise = (n,(c:cs))
        parseDigit n [] = (n, [])

data Heading = HeadingN | HeadingE | HeadingS | HeadingW
  deriving (Show, Eq)

turn HeadingN TurnL = HeadingW
turn HeadingW TurnL = HeadingS
turn HeadingS TurnL = HeadingE
turn HeadingE TurnL = HeadingN

turn HeadingN TurnR = HeadingE
turn HeadingE TurnR = HeadingS
turn HeadingS TurnR = HeadingW
turn HeadingW TurnR = HeadingN

data Face = FaceFront | FaceRear | FaceLeft | FaceRight | FaceTop | FaceBottom
  deriving (Show)

run :: Map (Int,Int) MapCell -> [Step] -> (Int,Int) -> Heading -> (((Int,Int),Heading),[((Int, Int),Heading)])
run grid steps = runSteps steps [] 
  where
    runSteps :: [Step] -> [((Int,Int),Heading)] -> (Int,Int) -> Heading -> (((Int,Int),Heading),[((Int, Int),Heading)])
    runSteps [] path position direction = ((position, direction), path)
    runSteps (step:steps) path position direction = let (position', direction', path') = case step of
                                                                                           StepTurn stepTurn -> (position, turn direction stepTurn, [(position, turn direction stepTurn)])
                                                                                           StepWalk stepWalk -> let (position', pathPoints) = walk direction position stepWalk
                                                                                                                 in (position', direction, (,direction) <$> pathPoints)
                                                     in runSteps steps (path' ++ path) position' direction'

    walk :: Heading -> (Int,Int) -> Int -> ((Int, Int), [(Int,Int)])
    walk direction position = go [position] position
      where
        (stepX,stepY) = case direction of
                          HeadingN -> (0,-1)
                          HeadingE -> (1,0)
                          HeadingS -> (0,1)
                          HeadingW -> (-1,0)
        takeStep (x,y) = let p' = wrap (x + stepX, y + stepY)
                          in case Map.lookup p' grid of
                               Nothing -> takeStep p'
                               _ -> p'
        wrap (x,y) | x >= maxX = (0, y)
                   | x < 0 = (maxX - 1, y)
                   | y >= maxY = (x, 0)
                   | y < 0 = (x, maxY - 1)
                   | otherwise = (x, y)
        go points p 0 = (p, points)
        go points p distance = let p' = takeStep p
                                in trace ("lots" ++ show (points, p, (stepX, stepY), direction, distance, p')) $ case Map.lookup p' grid of
                                     Just MapCellWall -> (p, points)
                                     Just MapCellSpace -> go (p':points) p' (distance - 1)
    
    minX = minimum . map fst . Map.keys $ grid
    minY = minimum . map snd . Map.keys $ grid
    maxX = maximum . map fst . Map.keys $ grid
    maxY = maximum . map snd . Map.keys $ grid


drawMap grid path = unlines $ [ [ case lookup (col,row) path of
                                    Just HeadingN -> '^'
                                    Just HeadingE -> '>'
                                    Just HeadingS -> 'v'
                                    Just HeadingW -> '<'
                                    Nothing -> case Map.lookup (col, row) grid of
                                                 Just MapCellWall -> '#'
                                                 Just MapCellSpace -> '.'
                                                 Nothing -> ' '
                                | col <- [minX..maxX]
                                ]
                              | row <- [minY..maxY]
                              ]
  where
    minX = minimum . map fst . Map.keys $ grid
    minY = minimum . map snd . Map.keys $ grid
    maxX = maximum . map fst . Map.keys $ grid
    maxY = maximum . map snd . Map.keys $ grid


part1 :: Parsed -> IO ()
part1 (grid, steps) = let startPos = head [(col, 0) | col <- [0..], Map.lookup (col, 0) grid == Just MapCellSpace]
                          (((col,row),heading), path) = run grid steps startPos HeadingE
                       in do putStrLn $ show startPos
                             putStrLn $ show steps
                             putStrLn $ show ((col, row),heading)
                             putStrLn $ show $ 1000 * (1 + row) + 4 * (1 + col) + (case heading of
                                                                                     HeadingE -> 0
                                                                                     HeadingS -> 1
                                                                                     HeadingW -> 2
                                                                                     HeadingN -> 3)
                             putStrLn $ drawMap grid path
                           

run2 :: Map (Int,Int) MapCell -> [Step] -> (Int,Int) -> Heading -> (((Int,Int),Heading),[((Int, Int),Heading)])
run2 grid steps = runSteps steps [] 
  where
    runSteps :: [Step] -> [((Int,Int),Heading)] -> (Int,Int) -> Heading -> (((Int,Int),Heading),[((Int, Int),Heading)])
    runSteps [] path position direction = ((position, direction), path)
    runSteps (step:steps) path position direction = let (position', direction', path') = case step of
                                                                                           StepTurn stepTurn -> (position, turn direction stepTurn, [(position, turn direction stepTurn)])
                                                                                           StepWalk stepWalk -> walk direction position stepWalk
                                                     in runSteps steps (path' ++ path) position' direction'

    walk :: Heading -> (Int,Int) -> Int -> ((Int, Int), Heading, [((Int,Int),Heading)])
    walk direction position 0 = (position, direction, [])
    walk direction position distance = let (p',d') = walk1 direction position
                                        in case Map.lookup p' grid of
                                             Just MapCellWall -> (position, direction, [(position, direction)])
                                             Just MapCellSpace -> let (p'', d'', path) = walk d' p' (distance - 1)
                                                                   in (p'', d'', (position, direction):path)
                                             Nothing -> error $ show (position, direction, faceFromCoords position, faceCoords position, p', d')
    walk1 direction position = go (faceFromCoords position) (faceCoords position)
      where
        go FaceTop (x,y)    | x == 0 && direction == HeadingW              = jumpF FaceLeft HeadingE (x,y)
                            | y == 0 && direction == HeadingN              = jumpS FaceRear HeadingE (x,y)
                            | otherwise                                    = (coordsFromFace FaceTop (x + stepX, y + stepY), direction)
        go FaceRight (x,y)  | y == 0 && direction == HeadingN              = jumpS FaceRear HeadingN (x,y)
                            | y == faceHeight - 1 && direction == HeadingS = jumpS FaceFront HeadingW (x,y)
                            | x == faceWidth - 1 && direction == HeadingE  = jumpF FaceBottom HeadingW (x,y)
                            | otherwise                                    = (coordsFromFace FaceRight (x + stepX, y + stepY), direction)
        go FaceFront (x,y)  | x == 0 && direction == HeadingW              = jumpS FaceLeft HeadingS (x,y)
                            | x == faceWidth - 1 && direction == HeadingE  = jumpS FaceRight HeadingN (x,y)
                            | otherwise                                    = (coordsFromFace FaceFront (x + stepX, y + stepY), direction)
        go FaceLeft (x,y)   | x == 0 && direction == HeadingW              = jumpF FaceTop HeadingE (x,y)
                            | y == 0 && direction == HeadingN              = jumpS FaceFront HeadingE (x,y)
                            | otherwise                                    = (coordsFromFace FaceLeft (x + stepX, y + stepY), direction)
        go FaceBottom (x,y) | x == faceWidth - 1 && direction == HeadingE  = jumpF FaceRight HeadingW (x,y)
                            | y == faceHeight - 1 && direction == HeadingS = jumpS FaceRear HeadingW (x,y)
                            | otherwise                                    = (coordsFromFace FaceBottom (x + stepX, y + stepY), direction)
        go FaceRear (x,y)   | x == 0 && direction == HeadingW              = jumpS FaceTop HeadingS (x,y)
                            | x == faceWidth - 1 && direction == HeadingE  = jumpS FaceBottom HeadingN (x,y)
                            | y == faceHeight - 1 && direction == HeadingS = jumpS FaceRight HeadingS (x,y)
                            | otherwise                                    = (coordsFromFace FaceRear (x + stepX, y + stepY), direction)
        (stepX,stepY) = case direction of
                          HeadingN -> (0,-1)
                          HeadingE -> (1,0)
                          HeadingS -> (0,1)
                          HeadingW -> (-1,0)
        jumpS newFace newDirection (x,y) = case (direction, newDirection) of
                                             (HeadingN, HeadingE) -> (coordsFromFace newFace (y,x), newDirection)
                                             (HeadingE, HeadingN) -> (coordsFromFace newFace (y,x), newDirection)
                                             (HeadingS, HeadingW) -> (coordsFromFace newFace (y,x), newDirection)
                                             (HeadingW, HeadingS) -> (coordsFromFace newFace (y,x), newDirection)
                                             (HeadingN, HeadingN) -> (coordsFromFace newFace (x,faceHeight - 1), newDirection)
                                             (HeadingS, HeadingS) -> (coordsFromFace newFace (x,0), newDirection)
        jumpF newFace newDirection (x,y) = case (direction, newDirection) of
                                             (HeadingW, HeadingE) -> (coordsFromFace newFace (x,faceHeight - y - 1), newDirection)
                                             (HeadingE, HeadingW) -> (coordsFromFace newFace (x,faceHeight - y - 1), newDirection)
    --        E         F          H
    --           TOP       RIGHT
    --        A         B          D
    --           FRONT
    -- A      C         D
    --   LEFT    BOTTOM  
    -- E      G         H 
    --   REAR
    -- F      H
    faceFromCoords (x, y) = case slice (x,y) of
                              (1,0) -> FaceTop
                              (2,0) -> FaceRight
                              (1,1) -> FaceFront
                              (1,2) -> FaceBottom
                              (0,2) -> FaceLeft
                              (0,3) -> FaceRear
                              (x',y') -> error $ show (faceWidth,faceHeight,(x,y),(x',y'), x `mod` faceWidth, y `mod` faceHeight)

    coordsFromFace face (x,y) = let (xFace, yFace) = case face of
                                                       FaceTop    -> (1,0) 
                                                       FaceRight  -> (2,0) 
                                                       FaceFront  -> (1,1) 
                                                       FaceBottom -> (1,2)
                                                       FaceLeft   -> (0,2) 
                                                       FaceRear   -> (0,3) 
                                 in (xFace * faceWidth + x, yFace * faceHeight + y)


    slice (x, y) = (x `div` faceWidth,y `div` faceHeight)
    faceCoords (x, y) = (x `mod` faceWidth,y `mod` faceHeight)
    faceWidth = (maxX + 1) `div` 3
    faceHeight = (maxY + 1) `div` 4
   
    minX = minimum . map fst . Map.keys $ grid
    minY = minimum . map snd . Map.keys $ grid
    maxX = maximum . map fst . Map.keys $ grid
    maxY = maximum . map snd . Map.keys $ grid


demoMode = let emptyWorld = Map.fromList [((x,y),MapCellSpace)
                                         | (x,y) <- [(x,y) | x <- [4..11], y <- [0..3]] ++
                                                    [(x,y) | x <- [4..7], y <- [4..7]] ++
                                                    [(x,y) | x <- [0..7], y <- [8..11]] ++
                                                    [(x,y) | x <- [0..3], y <- [11..15]]]
            in forM_ [(5,1),(9,1),(5,5),(5,9),(1,9),(1,13)] $ \midpoint ->
                 forM_ [HeadingN,HeadingE,HeadingS,HeadingW] $ \heading ->
                   let (_,path) = run2 emptyWorld [StepWalk 5] midpoint heading
                    in putStrLn $ drawMap emptyWorld path

part2 :: Parsed -> IO ()
part2 (grid, steps) = do -- demoMode
                         let startPos = head [(col, 0) | col <- [0..], Map.lookup (col, 0) grid == Just MapCellSpace]
                         let (((col,row),heading), path) = run2 grid steps startPos HeadingE
                         putStrLn $ show startPos
                         putStrLn $ show steps
                         putStrLn $ show ((col, row),heading)
                         putStrLn $ show $ 1000 * (1 + row) + 4 * (1 + col) + (case heading of
                                                                                 HeadingE -> 0
                                                                                 HeadingS -> 1
                                                                                 HeadingW -> 2
                                                                                 HeadingN -> 3)
                         putStrLn $ drawMap grid path

day22 part args = do
  let filename = case args of
                   [] -> "inputs/day22"
                   [f] -> f
  inputs <- parseInput <$> readFile filename
  case part of
    Part1 -> part1 inputs
    Part2 -> part2 inputs
