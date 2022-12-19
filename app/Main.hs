module Main where

import Data.Maybe
import System.Environment (getArgs)
import System.Exit (die)

-- NEXT DAY IMPORT

import Part (getPart, Part (Part1, Part2))

getDay :: String -> Maybe (Part -> [String] -> IO ())
getDay n =
  case n of
    -- NEXT DAY CASE
    _ -> Nothing


main :: IO ()
main = do
  args <- getArgs
  case args of
    (day:part:rest) -> 
      do
        day' <- fromMaybe (die $ "Unknown day " ++ day) (pure <$> getDay day)
        part' <- fromMaybe (die $ "Unknown part " ++ part) (pure <$> getPart part)
        (day' :: Part -> [String] -> IO ()) part' rest
    _ -> putStrLn "usage: <cmd> <day> <part>"
