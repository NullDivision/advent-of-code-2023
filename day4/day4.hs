module Main where

import Data.Word (Word8, Word16)

import Debug.Trace

data Card = Card { gameId :: Word8, matches :: Word8, qty :: Int }
  deriving (Show)

getWinningNumbers :: [Word8] -> [Word8] -> Word8 
getWinningNumbers w n = fromIntegral . length $ filter (\i -> i `elem` w) n

-- Card contains a list of winning numbers, a list of game numbers, and a multiplier
-- Multiplier is used for subsequent games to decide how many times to apply effect
lineToCard :: String -> Card
lineToCard l =
  let (w, n) = break (\c -> c == '|') $ drop 10 l
  in (Card (read . init . last . words $ take 10 l) (getWinningNumbers (map (\w -> read w :: Word8) $ words w) (map (\w -> read w :: Word8) . words $ drop 1 n)) 1)

processCards :: [Card] -> [Card] -> [Card]
processCards c [] = c
processCards c (s:sx) = processCards (c ++ [s]) $ map (\(Card a b c) -> Card a b (c+(qty s))) (take (read $ show (matches s) :: Int) sx) ++ (drop (read $ show (matches s) :: Int) sx)

countCards :: Int -> [Card] -> Int
countCards c [] = c
countCards c (s:sx) = countCards (c + (qty s)) sx

main = do
  values <- readFile "values.txt"

  let cards = map lineToCard . lines $ values

  print "Sqared total:"
  print . foldr (+) 0 . map (\v ->
    let c = matches v
    in if c == 0 then 0 else 2^(c-1)) $ cards
  print "Compounded total:"
  print . countCards 0 . processCards [] $ cards
