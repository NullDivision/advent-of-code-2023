splitSets :: Char -> String -> [String]
splitSets _ [] = []
splitSets sep str =
  let (left, right) = break (== sep) str
  -- drop both separator and trailing whitespace
  -- unreliable but I can't be bothered
  in left : splitSets sep (drop 2 right)

toInt :: String -> Integer
toInt s = read s :: Integer

getLastWord :: String -> String
getLastWord s = last $ words s

toRgb :: (String, String, String) -> [String] -> (String, String, String)
toRgb c [] = c
toRgb (r, g, b) (s : sx)
  | "red" == (getLastWord s) = toRgb (s, g, b) sx
  | "green" == (getLastWord s) = toRgb (r, s, b) sx
  | "blue" == (getLastWord s) = toRgb (r, g, s) sx
  | otherwise = toRgb (r, g, b) sx

-- function gets called with a color set (eg. "1 red, 2 green, 3 blue")
splitRgb :: String -> [(String, String, String)]
splitRgb s = map(toRgb ("0", "0", "0") . words) . splitSets ',' $ s

splitParts :: [Char] -> (String, [[(String, String, String)]])
splitParts s =
  let (game, cubes) = break (== ':') s
  -- we drop the ': ' part from cubes
  in (
    game,
    map(splitRgb) (splitSets ';' (drop 2 cubes))
  )

main = do
  values <- readFile "./values.txt"

  print . fmap(splitParts) . lines $ values
