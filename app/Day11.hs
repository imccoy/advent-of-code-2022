module Day11 (day11) where

import Control.Monad (forM_)
import Data.List (intercalate, sort)
import Text.Parsec

import Part (Part (Part1, Part2))

class WorryLevel a where
  mulWorry :: a -> Int -> a
  addWorry :: a -> Int -> a
  squareWorry :: a -> a
 
  divisibleWorry :: a -> Int -> Bool
  reduceWorries :: a -> a

data SimpleWorryLevel = SimpleWorryLevel Int
  deriving (Show)

instance WorryLevel SimpleWorryLevel where
  mulWorry (SimpleWorryLevel n) n' = SimpleWorryLevel $ n * n'
  addWorry (SimpleWorryLevel n) n' = SimpleWorryLevel $ n + n'
  squareWorry (SimpleWorryLevel n) = SimpleWorryLevel $ n * n

  divisibleWorry (SimpleWorryLevel n) n' = n `mod` n' == 0
  reduceWorries (SimpleWorryLevel n) = SimpleWorryLevel $ n `div` 3


data IrreducibleWorryLevel = IrreducibleWorryLevel Int
  deriving (Show)

instance WorryLevel IrreducibleWorryLevel where
  mulWorry (IrreducibleWorryLevel n) n' = IrreducibleWorryLevel $ n * n'
  addWorry (IrreducibleWorryLevel n) n' = IrreducibleWorryLevel $ n + n'
  squareWorry (IrreducibleWorryLevel n) = IrreducibleWorryLevel $ n * n

  divisibleWorry (IrreducibleWorryLevel n) n' = n `mod` n' == 0
  reduceWorries (IrreducibleWorryLevel n) = IrreducibleWorryLevel n


newtype Base = Base Int
  deriving (Show)
baseNum (Base n) = n

data PrimalWorryLevel = PrimalWorryLevel Base [Int]

addW base = go 0
  where go carry (a:as) (b:bs) = let n = a + b + carry
                                  in if n > baseNum base
                                       then (n - baseNum base):(go 1 as bs)
                                       else n:(go 0 as bs)
        go 0 as [] = as
        go 0 [] bs = bs
        go carry as [] = go carry as [0]
        go carry [] bs = go carry [0] bs

mulWs base = go []
  where go leadingZeroes (a:as) n' = let a' = a * n'
                                     in addW base
                                          (go (0:leadingZeroes) as n') $
                                          if a' > baseNum base
                                            then leadingZeroes ++ [a' `mod` baseNum base, a' `div` baseNum base]
                                            else leadingZeroes ++ [a']
        go leadingZeroes [] b = [0]

mulW base a = go []
  where go leadingZeroes (b:bs) = addW base (go (0:leadingZeroes) bs)
                                            (leadingZeroes ++ mulWs base a b)
        go _ [] = [0]

instance WorryLevel PrimalWorryLevel where
  addWorry (PrimalWorryLevel b n) n' = PrimalWorryLevel b $ addW b n [n']
  mulWorry (PrimalWorryLevel b n) n' = PrimalWorryLevel b $ mulW b n [n']
  squareWorry (PrimalWorryLevel b n) = PrimalWorryLevel b $ mulW b n n

  divisibleWorry (PrimalWorryLevel b (0:_)) _ = True
  divisibleWorry (PrimalWorryLevel b (n:_)) n' = n `mod` n' == 0
  reduceWorries (PrimalWorryLevel b n) = PrimalWorryLevel b n



data Monkey a = Monkey { monkeyNum :: Int, monkeyWorryLevels :: [a], monkeyOperation :: (Operation,Operand), monkeyTestDivisor :: Int, monkeyTrueTarget :: Int, monkeyFalseTarget :: Int, inspectionCount :: Int, roundNumber :: Int }
  deriving (Show)
type Parsed a = [Monkey a]

alterWorryLevels f = map (\monkey -> Monkey { monkeyNum = monkeyNum monkey
                                            , monkeyWorryLevels = map f $ monkeyWorryLevels monkey
                                            , monkeyOperation = monkeyOperation monkey
                                            , monkeyTestDivisor = monkeyTestDivisor monkey
                                            , monkeyTrueTarget = monkeyTrueTarget monkey
                                            , monkeyFalseTarget = monkeyFalseTarget monkey
                                            , inspectionCount = inspectionCount monkey
                                            , roundNumber = roundNumber monkey
                                            })

data Operand = OpVarOld | OperandInt Int
  deriving (Show)
data Operation = OperationPlus | OperationMul
  deriving (Show)

parseOperand = (string "old" >> pure OpVarOld) <|> (OperandInt . read <$> many1 digit)
parseOperator = (char '+' >> pure OperationPlus) <|> (char '*' >> pure OperationMul)

parseOperation = do
  string "new = old "
  operator <- parseOperator
  char ' '
  b <- parseOperand
  pure (operator, b)

parseMonkey = do
  string "Monkey "
  monkeyNum <- read . pure <$> digit
  char ':'
  char '\n'

  string "  Starting items: "
  worryLevels <- map (SimpleWorryLevel . read) <$> sepBy (try $ many1 digit) (string ", ")
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

parseInput :: String -> [Monkey SimpleWorryLevel]
parseInput = either (error . show) id . runParser parseMonkeys () "none"

runInspectionOperation (OperationPlus, OperandInt n) worryLevel = addWorry worryLevel n
runInspectionOperation (OperationMul, OperandInt n) worryLevel = mulWorry worryLevel n
runInspectionOperation (OperationMul, OpVarOld) worryLevel = squareWorry worryLevel

runMonkeys :: (WorryLevel a) => [Monkey a] -> [Monkey a]
runMonkeys (monkey:monkeys) = let worryLevels = monkeyWorryLevels monkey
                                  throws = [ (if divisibleWorry newWorryLevel (monkeyTestDivisor monkey) then monkeyTrueTarget monkey else monkeyFalseTarget monkey
                                             ,newWorryLevel)
                                           | worryLevel <- worryLevels
                                           , let newWorryLevel = reduceWorries $ runInspectionOperation (monkeyOperation monkey) worryLevel
                                           ]
                                  newInspectionCount = (inspectionCount monkey) + fromIntegral (length worryLevels)

                                  newMonkey = newInspectionCount `seq`
                                              monkey { monkeyWorryLevels = []
                                                     , inspectionCount = newInspectionCount
                                                     , roundNumber = (roundNumber monkey) + 1
                                                     }
                               in newMonkey:(runMonkeys $ [ other { monkeyWorryLevels = (monkeyWorryLevels other) ++ received }
                                                          | other <- monkeys
                                                          , let received = [ throwLevel 
                                                                           | (throwTarget, throwLevel) <- throws
                                                                           , throwTarget == monkeyNum other]
                                                          ] ++ [newMonkey])

part1 :: Parsed SimpleWorryLevel -> IO ()
part1 = putStrLn . show . product . take 2 . reverse . sort . map inspectionCount . takeWhile ((== 20) . roundNumber) . dropWhile ((< 20) . roundNumber) . runMonkeys

part2 :: Parsed SimpleWorryLevel -> IO ()
part2 monkeys = do
  putStrLn $ "100 + 23" ++ show (addW (Base 10) [0,0,1] [3,2])
  putStrLn $ "99 + 24" ++ show (addW (Base 10) [9,9] [4,2])
  putStrLn $ "11 * 7" ++ show (mulWs (Base 10) [1,1] 7)
  putStrLn $ "13 * 9" ++ show (mulWs (Base 10) [3,1] 9)
  putStrLn $ "13 * 99" ++ show (mulW (Base 10) [3,1] [9,9])
  let base = fromIntegral $ product (monkeyTestDivisor <$> monkeys)
  let monkeysInBase = alterWorryLevels (\(SimpleWorryLevel a) -> PrimalWorryLevel (Base base) [a]) monkeys

  putStrLn . show . map inspectionCount . takeWhile ((== 1000) . roundNumber) . dropWhile ((< 1000) . roundNumber) . runMonkeys $ monkeysInBase
  putStrLn . show . product . take 2 . reverse . sort . map inspectionCount . takeWhile ((== 20) . roundNumber) . dropWhile ((< 20) . roundNumber) . runMonkeys $ monkeysInBase

day11 part args = do
  let filename = case args of
                   [] -> "inputs/day11"
                   [f] -> f
  inputs <- parseInput <$> readFile filename
  case part of
    Part1 -> part1 inputs
    Part2 -> part2 inputs
