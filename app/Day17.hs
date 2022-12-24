{-# LANGUAGE LambdaCase #-}
module Day17 (day17) where

import Control.Monad (forM_, mapM_)
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import qualified Data.Map as Map

import Part (Part (Part1, Part2))

data Jet = JetL | JetR
  deriving (Show)
parseJet '>' = Just JetR
parseJet '<' = Just JetL
parseJet '\n' = Nothing

data Pix = PixY | PixN
  deriving (Show,Eq,Ord)

allShapes = [ [[],[],[],[PixY,PixY,PixY,PixY]]
            , [[],[PixN,PixY],[PixY,PixY,PixY],[PixN,PixY]]
            , [[],[PixN,PixN,PixY],[PixN,PixN,PixY],[PixY,PixY,PixY]]
            , [[PixY],[PixY],[PixY],[PixY]]
            , [[],[],[PixY,PixY],[PixY,PixY]]
            ]

emptyRow = replicate 7 PixN

pushSideways world coord direction shape = if coord + offset >= 0 && fitsAt shape (coord + offset) world
                                             then coord + offset
                                             else coord
  where
    offset = case direction of
               JetL -> -1
               JetR -> 1

fitsAt shape coord = fits shape . map (drop coord)

fits (shapeRow:shapeRows) (worldRow:worldRows) = fits1 shapeRow worldRow && fits shapeRows worldRows
  where fits1 (PixN:shapeCells) (_:worldCells) = fits1 shapeCells worldCells
        fits1 (PixY:shapeCells) (PixN:worldCells) = fits1 shapeCells worldCells
        fits1 (PixY:shapeCells) (PixY:worldCells) = False
        fits1 [] _ = True -- empty shape fits
        fits1 (_:_) [] = False -- oops, we ran out of world
fits [] _ = True -- empty shape fits
fits (_:_) [] = False -- oops, we ran out of world

installAt coord shape world = install ((replicate coord PixN ++) <$> shape) world

install (shapeRow:shapeRows) (worldRow:worldRows) = (install1 shapeRow worldRow):(install shapeRows worldRows)
  where install1 (PixN:shapeCells) (worldCell:worldCells) = worldCell:(install1 shapeCells worldCells)
        install1 (PixY:shapeCells) (_:worldCells) = PixY:(install1 shapeCells worldCells)
        install1 [] worldCells = worldCells
        install1 _ [] = [] -- out of bounds, just return empty
install [] worldRows = worldRows
install _ [] = [] -- out of bounds, just return empty

fall shape world@(topRow:nextWorld) coord (direction:directions) = if fitsAt shape coord' nextWorld
                                                                     then let (rows, directions') = fall shape nextWorld coord' directions
                                                                           in (topRow:rows, directions')
                                                                     else (installAt coord' shape world, directions)
  where coord' = pushSideways world coord direction shape


repad world = let newRows = 3 - (length (takeWhile (== emptyRow) world) - 4)
               in (replicate newRows emptyRow ++ world, fromIntegral $ newRows)

printWorld rows = do putStrLn "======================"
                     forM_ rows $ \row -> do
                       putStrLn $ "|" ++ rowString row ++ "|"
rowString = map (\case
                   PixY -> '#'
                   PixN -> '.'
                )

run 0 world rowCount shapes directions = (world, rowCount, shapes, directions)
run steps world rowCount (shape:shapes) directions = let (world', directions') = fall shape world 2 directions
                                                         (world'', rows) = repad world'
                                                         rowCount' = rows + rowCount
                                                      in rowCount' `seq` run (steps - 1) world'' rowCount' shapes directions'

runBig n big world rowCount shapes directions = let (world', rowCount', shapes', directions') = run big world rowCount shapes directions
                                                 in (n,rowCount',world'):(runBig (n+1) big world' rowCount' shapes' directions')

type Parsed = [Jet]

parseInput :: String -> Parsed
parseInput = mapMaybe parseJet

part1 :: Parsed -> IO ()
part1 directions = do putStrLn $ show $ length directions
                      let (world, rowCount, _, _) = run 2022 (replicate 7 emptyRow) 0 (cycle allShapes) (cycle directions)
                      putStrLn $ show $ length world
                      putStrLn $ show $ rowCount


findLoop ((n,nRowCount,nWorld):worlds) worldsMap = case Map.lookup (take 300 nWorld) worldsMap of
                                                     Just (m,mRowCount,mWorld) -> do putStrLn $ show (m, mRowCount, mWorld)
                                                                                     pure (n, m, mWorld, mRowCount, nRowCount - mRowCount)
                                                     Nothing -> do putStrLn $ "Not yet " ++ show n
                                                                   findLoop worlds (Map.insert (take 300 nWorld) (n,nRowCount,nWorld) worldsMap)
part2 :: Parsed -> IO ()
part2 directions = do let loopSize = fromIntegral $ length allShapes * length directions
                      putStrLn $ "loopSize" ++ (show loopSize)
                      (lastIteration, firstIteration, firstWorld, rowsToLoopStart, rowsInLoop) <- findLoop (runBig (0 :: Integer) loopSize (replicate 7 emptyRow) 0 (cycle allShapes) (cycle directions)) Map.empty
                      putStrLn $ "firstIteration" ++ (show firstIteration)
                      putStrLn $ "lastIteration" ++ (show lastIteration)
                      putStrLn $ "rowsToLoopStart" ++ (show rowsToLoopStart)
                      putStrLn $ "rowsInLoop" ++ (show rowsInLoop)
                      -- a mega iteration goes from immediately after the first regular iteration that ends with The Pattern until the end of the
                      -- second regular iteration that ends with The Pattern
                      let megaIterationSize = loopSize * (lastIteration - firstIteration)
                      putStrLn $ "megaIterationSize" ++ (show megaIterationSize)
                      let hungryHungry = 1000000000000
                      let numMegaIterations = (hungryHungry - firstIteration) `div` megaIterationSize
                      putStrLn $ "numMegaIterations" ++ (show numMegaIterations)
                      let remainingAfterMegaIterations = (hungryHungry - firstIteration) `mod` megaIterationSize
                      putStrLn $ "remainingAfterMegaIterations" ++ (show remainingAfterMegaIterations)
                      let (_,extraRows, _, _) = run remainingAfterMegaIterations firstWorld 0 (cycle allShapes) (cycle directions)
                      putStrLn $ "extraRows" ++ (show extraRows)
                      putStrLn $ show $ numMegaIterations * rowsInLoop + extraRows

day17 part args = do
  let filename = case args of
                   [] -> "inputs/day17"
                   [f] -> f
  inputs <- parseInput <$> readFile filename
  case part of
    Part1 -> part1 inputs
    Part2 -> part2 inputs
