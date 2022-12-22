module Day13 (day13) where
import Part (Part (Part1, Part2))

import Control.Monad (void)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Text.Parsec

type Parsed = [(Packet,Packet)]

data Packet = PacketInt Int | PacketList [Packet]
  deriving (Show, Eq)

parsePackets = sepBy parsePacketPair (char '\n')

parsePacketPair = do l <- parsePacket
                     char '\n'
                     r <- parsePacket
                     char '\n'
                     pure (l,r)
                     
parsePacket = parseList <|> parseInt
  where parseList = do void $ char '['
                       packets <- sepBy parsePacket (string ",")
                       void $ char ']'
                       pure $ PacketList packets
        parseInt = do digits <- many1 digit
                      pure $ PacketInt (read digits)

parseInput :: String -> Parsed
parseInput = either (error . show) id . runParser parsePackets () "none"

data RW = RWRight | RWWrong | RWUnknown
  deriving (Eq)

rightOrder (PacketInt l) (PacketInt r) | l < r = RWRight
                                       | l > r = RWWrong
                                       | otherwise = RWUnknown
rightOrder (PacketList (l:ls)) (PacketList (r:rs)) = case rightOrder l r of
                                                       RWUnknown -> rightOrder (PacketList ls) (PacketList rs)
                                                       definitely -> definitely
rightOrder (PacketList []) (PacketList (_:_)) = RWRight
rightOrder (PacketList (_:_)) (PacketList []) = RWWrong
rightOrder (PacketList []) (PacketList []) = RWUnknown
rightOrder (PacketList ls) (PacketInt r) = rightOrder (PacketList ls) (PacketList [PacketInt r])
rightOrder (PacketInt l) (PacketList rs) = rightOrder (PacketList [PacketInt l]) (PacketList rs)

part1 :: Parsed -> IO ()
part1 = putStrLn . show . sum . map fst . filter (\(_, (l,r)) -> rightOrder l r == RWRight) . zip [1..]

insertPacket packet [] = [packet]
insertPacket packet (p:ps) = case rightOrder packet p of
                               RWRight -> packet:p:ps
                               _ -> p:(insertPacket packet ps)

part2 :: Parsed -> IO ()
part2 packetPairs = do
  let divider1 = PacketList [PacketList [PacketInt 2]]
  let divider2 = PacketList [PacketList [PacketInt 6]]
  let allPackets = divider1:divider2:[packet | (p1, p2) <- packetPairs, packet <- [p1, p2]]
  let sortedList = foldr insertPacket [] allPackets
  putStrLn . show $ sortedList
  putStrLn . show $ (fromMaybe 0 (findIndex (\x -> x == divider1) sortedList) + 1) * (fromMaybe 0 (findIndex (\x -> x == divider2) sortedList) + 1)
  

day13 part args = do
  let filename = case args of
                   [] -> "inputs/day13"
                   [f] -> f
  inputs <- parseInput <$> readFile filename
  case part of
    Part1 -> part1 inputs
    Part2 -> part2 inputs
