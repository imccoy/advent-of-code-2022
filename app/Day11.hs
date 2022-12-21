module Day11 (day11) where

import Data.List (sort)
import Text.Parsec

import Part (Part (Part1, Part2))

data Monkey = Monkey { monkeyNum :: Int, monkeyWorryLevels :: [Int], monkeyOperation :: (Operand,Operation,Operand), monkeyTestDivisor :: Int, monkeyTrueTarget :: Int, monkeyFalseTarget :: Int, inspectionCount :: Int, roundNumber :: Int }
  deriving (Show)
type Parsed = [Monkey]

data OpVar = OpVarNew | OpVarOld
  deriving (Show)
data Operand = OperandVar OpVar | OperandInt Int
  deriving (Show)
data Operation = OperationPlus | OperationMul
  deriving (Show)

parseOpVar = (string "new" >> pure OpVarNew) <|> (string "old" >> pure OpVarOld)
parseOperand = (OperandVar <$> parseOpVar) <|> (OperandInt . read <$> many1 digit)
parseOperator = (char '+' >> pure OperationPlus) <|> (char '*' >> pure OperationMul)

parseOperation = do
  string "new = "
  a <- parseOperand
  char ' '
  operator <- parseOperator
  char ' '
  b <- parseOperand
  pure (a, operator, b)

parseMonkey = do
  string "Monkey "
  monkeyNum <- read . pure <$> digit
  char ':'
  char '\n'

  string "  Starting items: "
  worryLevels <- map read <$> sepBy (try $ many1 digit) (string ", ")
  char '\n'

  string "  Operation: "
  operation <- parseOperation
  char '\n'

  string "  Test: divisible by "
  condition <- read <$> many1 digit
  char '\n'

  string "    If true: throw to monkey "
  conditionTrueMonkeyNum <- read . pure <$> digit
  char '\n'
  
  string "    If false: throw to monkey "
  conditionFalseMonkeyNum <- read . pure <$> digit
  char '\n'

  pure $ Monkey { monkeyNum = monkeyNum, monkeyWorryLevels = worryLevels, monkeyOperation = operation, monkeyTestDivisor = condition, monkeyTrueTarget = conditionTrueMonkeyNum, monkeyFalseTarget = conditionFalseMonkeyNum, inspectionCount = 0, roundNumber = 0 }

parseMonkeys = sepBy parseMonkey (char '\n')

parseInput :: String -> [Monkey]
parseInput = either (error . show) id . runParser parseMonkeys () "none"

runInspectionOperation (a, op, b) worryLevel = let a' = resolve a
                                                   b' = resolve b
                                                in case op of
                                                     OperationPlus -> a' + b'
                                                     OperationMul -> a' * b'
  where resolve (OperandVar OpVarOld) = worryLevel
        resolve (OperandInt n) = n

runMonkeys :: (Int -> Int) -> [Monkey] -> [Monkey]
runMonkeys relax (monkey:monkeys) = let worryLevels = monkeyWorryLevels monkey
                                        throws = [ (if newWorryLevel `mod` (monkeyTestDivisor monkey) == 0 then monkeyTrueTarget monkey else monkeyFalseTarget monkey
                                                   ,newWorryLevel)
                                                 | worryLevel <- worryLevels
                                                 , let newWorryLevel = relax (runInspectionOperation (monkeyOperation monkey) worryLevel)
                                                 ]
                                        newMonkey = monkey { monkeyWorryLevels = []
                                                           , inspectionCount = (inspectionCount monkey) + length worryLevels
                                                           , roundNumber = (roundNumber monkey) + 1
                                                           }
                                     in newMonkey:(runMonkeys relax $ [ other { monkeyWorryLevels = (monkeyWorryLevels other) ++ received }
                                                                      | other <- monkeys
                                                                      , let received = [ throwLevel 
                                                                                       | (throwTarget, throwLevel) <- throws
                                                                                       , throwTarget == monkeyNum other]
                                                                      ] ++ [newMonkey])

part1 :: Parsed -> IO ()
part1 = putStrLn . show . product . take 2 . reverse . sort . map inspectionCount . takeWhile ((== 20) . roundNumber) . dropWhile ((< 20) . roundNumber) . runMonkeys (`div` 3)

part2 :: Parsed -> IO ()
part2 monkeys = do
  putStrLn . show . product . take 2 . reverse . sort . map inspectionCount . takeWhile ((== 10000) . roundNumber) . dropWhile ((< 10000) . roundNumber) . runMonkeys (`mod` (product [monkeyTestDivisor monkey | monkey <- monkeys])) $ monkeys

day11 part args = do
  let filename = case args of
                   [] -> "inputs/day11"
                   [f] -> f
  inputs <- parseInput <$> readFile filename
  case part of
    Part1 -> part1 inputs
    Part2 -> part2 inputs
