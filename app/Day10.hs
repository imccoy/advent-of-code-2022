module Day10 (day10) where

import Control.Monad (forM_)
import Data.List.Split (splitOn, chunksOf)

import Part (Part (Part1, Part2))

data Instruction = AddX Int | Noop
parseInstruction line = case splitOn " " line of
                          ["addx",n] -> AddX (read n)
                          ["noop"]   -> Noop

type Parsed = [Instruction]

runInstruction reg Noop = ([reg],reg)
runInstruction reg (AddX op) = ([reg,reg],reg + op)
runInstructions reg [] = ([], reg)
runInstructions reg (instr:instrs) = let (vals,next) = runInstruction reg instr
                                         (vals',next') = runInstructions next instrs
                                      in (vals ++ vals', next')

interestingStrengths = filter (\(cycle, _) -> (cycle - 20) `mod` 40 == 0) . zip [1..]
readStrengths = map (\(cycle, reg) -> cycle * reg) . interestingStrengths

parseInput :: String -> Parsed
parseInput = fmap parseInstruction . lines

part1 :: Parsed -> IO ()
part1 instructions = do
  putStrLn . show . zip [1..] . fst . runInstructions 1 $ instructions
  putStrLn . show . interestingStrengths. fst . runInstructions 1 $ instructions
  putStrLn . show . sum . readStrengths . fst . runInstructions 1 $ instructions



part2 :: Parsed -> IO ()
part2 instructions = do
  let spritePositions = fst . runInstructions 1 $ instructions
  let litPixels = zipWith (\scanPos spritePos -> scanPos >= spritePos - 1 && scanPos <= spritePos + 1) (concat $ repeat [0..39]) spritePositions
  forM_ (chunksOf 40 litPixels) $ putStrLn . map (\pixel -> if pixel then '#' else '.')

day10 part args = do
  let filename = case args of
                   [] -> "inputs/day10"
                   [f] -> f
  inputs <- parseInput <$> readFile filename
  case part of
    Part1 -> part1 inputs
    Part2 -> part2 inputs
