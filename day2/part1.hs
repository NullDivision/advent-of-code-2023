splitSets :: Char -> String -> [String]
splitSets _ [] = []
splitSets sep str =
  let (left, right) = break (== sep) str
  -- drop both separator and trailing whitespace
  -- unreliable but I can't be bothered
  in left : splitSets sep (drop 2 right)

toInt :: String -> Integer
toInt s = read s :: Integer

getValue :: String -> String
getValue s = head $ words s

getLastWord :: String -> String
getLastWord s = last $ words s

toRgb :: (String, String, String) -> [String] -> (String, String, String)
toRgb c [] = c
toRgb (r, g, b) (s : sx)
  | "red" == (getLastWord s) = toRgb (getValue s, g, b) sx
  | "green" == (getLastWord s) = toRgb (r, getValue s, b) sx
  | "blue" == (getLastWord s) = toRgb (r, g, getValue s) sx
  | otherwise = toRgb (r, g, b) sx

-- function gets called with a color set (eg. "1 red, 2 green, 3 blue")
splitRgb :: String -> (String, String, String)
splitRgb s = toRgb ("0", "0", "0") . splitSets ',' $ s

toNumbers :: (String, String, String) -> (Integer, Integer, Integer)
toNumbers (r, g, b) = (read r, read g, read b)

isValidSet :: [(Integer, Integer, Integer)] -> Bool
isValidSet [] = True
isValidSet ((r, g, b) : sx)
  | r > 12 || g > 13 || b > 14 = False
  | otherwise = isValidSet sx

splitParts :: [Char] -> (Integer, [(Integer, Integer, Integer)])
splitParts s =
  let (game, cubes) = break (== ':') s
  -- we drop the ': ' part from cubes
  in (
    toInt $ getLastWord game,
    map(toNumbers . splitRgb) (splitSets ';' (drop 2 cubes))
  )

class RgbTuple a where
  combineMax :: a -> a -> a

pow (r, g, b) = r * g * b

instance Ord a => RgbTuple (a, a, a) where
  combineMax (r1, g1, b1) (r2, g2, b2) = (max r1 r2, max g1 g2, max b1 b2)

main = do
  values <- readFile "./values.txt"

  let parsedValues = fmap(splitParts) . lines $ values

  print "IDs total: "
  print $ show (foldr (+) 0 . map(\(g, x) -> g) . filter (\(g, x) -> isValidSet x) $ parsedValues)
  print "Power total: "
  print . foldr (+) 0 . map (\(_, x) -> pow $ foldr combineMax (0, 0, 0) x) $ parsedValues 
