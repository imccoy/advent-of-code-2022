module Day21 (day21) where

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

import Part (Part (Part1, Part2))

data Operator = Add | Subtract | Multiply | Divide | Eq
data Shout = ShoutConst Int | ShoutOp String Operator String

type Parsed = [(String, Shout)]

parseInput :: String -> Parsed
parseInput = map parseShoutLine . lines

parseShoutLine line = let [label, shoutString] = splitOn ": " line
                       in (label, parseShout shoutString)
  where parseShout shoutString = case splitOn " " shoutString of
                                   [aString, opString, bString] -> ShoutOp aString (parseOperator opString) bString
                                   [nString] -> ShoutConst $ read nString
        parseOperator "+" = Add
        parseOperator "-" = Subtract
        parseOperator "*" = Multiply
        parseOperator "/" = Divide

run1 monkeys = go "root"
  where go = go' . fromJust . (`Map.lookup` monkeys)
        go' (ShoutConst n) = n
        go' (ShoutOp a operator b) = runOperator (go a) operator (go b)

runOperator a Add b = a + b
runOperator a Subtract b = a - b
runOperator a Multiply b = a * b
runOperator a Divide b = a `div` b
                                               

part1 :: Parsed -> IO ()
part1 = putStrLn . show . run1 . Map.fromList


data UnHuman = Human (Int -> Int) | UnHuman Int
unHuman monkeys "humn" = Human (\n -> n)
unHuman monkeys label = case fromJust $ Map.lookup label monkeys of
                          ShoutConst n -> UnHuman n
                          ShoutOp left op right ->
                            case (unHuman monkeys left, op, unHuman monkeys right) of
                              (Human f,   Add, UnHuman n)      -> Human $ \desired -> f (desired - n)
                              (UnHuman n, Add, Human f)        -> Human $ \desired -> f (desired - n)
                              (Human f,   Multiply, UnHuman n) -> Human $ \desired -> f (desired `div` n)
                              (UnHuman n, Multiply, Human f)   -> Human $ \desired -> f (desired `div` n)
                              -- a -> b - c => b = a + c and c = b - a
                              (Human f,   Subtract, UnHuman n) -> Human $ \desired -> f (desired + n)
                              (UnHuman n, Subtract, Human f)   -> Human $ \desired -> f (n - desired)
                              -- a -> b / c => b = a * c and c = b / a
                              (Human f,   Divide, UnHuman n)   -> Human $ \desired -> f (desired * n)
                              (UnHuman n, Divide, Human f)     -> Human $ \desired -> f (n `div` desired)
                              (UnHuman a, op, UnHuman b)       -> UnHuman $ runOperator a op b

run2 monkeys = let (ShoutOp left _ right) = fromJust $ Map.lookup "root" monkeys
                in case (unHuman monkeys left, unHuman monkeys right) of
                     (Human f, UnHuman r) -> f r
                     (UnHuman l, Human f) -> f l
                

part2 :: Parsed -> IO ()
part2 = putStrLn . show . run2 . Map.fromList

day21 part args = do
  let filename = case args of
                   [] -> "inputs/day21"
                   [f] -> f
  inputs <- parseInput <$> readFile filename
  case part of
    Part1 -> part1 inputs
    Part2 -> part2 inputs
