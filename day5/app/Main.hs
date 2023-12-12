module Main where

import Data.List(find)

groupBySection :: [[String]] -> [String] -> [[String]]
groupBySection l [] = l
groupBySection l s =
  let (v, vx) = break (== "") s
  in groupBySection (l ++ [v]) (drop 1 vx)

toWord8 :: [Char] -> Int
toWord8 = read

toMapper :: [Int] -> (Int, Int, Int)
toMapper (_:_:_:_:_) = error "Invalid mapper"
toMapper [] = error "Invalid mapper"
toMapper [_] = error "Invalid mapper"
toMapper [_, _] = error "Invalid mapper"
toMapper [x, y, z] = (x, y, z)

withDefault :: Maybe (Int, Int, Int) -> Int -> Int
withDefault (Just (d, s, _)) v = d + (v - s)
withDefault _ b = b

calculate :: [[(Int, Int, Int)]] -> Int -> Int
calculate [] v = v
calculate (l:lx) v = calculate lx (withDefault (find (\(_, s, i) -> v >= s && v <= (s+i)) l) v)

main :: IO ()
main = do
  values <- readFile "values.txt"

  let valueLines = lines $ values
  let sections = groupBySection [] valueLines
  let mappingData = drop 1 sections
  let seeds = map (toWord8) $ words . drop 7 . head $ head sections
  let mappings = map (\l -> map (toMapper . map (toWord8) . words) $ drop 1 l) mappingData

  print $ foldr min (maxBound :: Int) $ map (\n -> calculate mappings n) seeds

