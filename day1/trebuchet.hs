import Data.Char
import Data.String
import Data.List

findFirstDigit :: [Char] -> Maybe Char
findFirstDigit = find (isDigit)

withDefault :: Maybe Char -> Char
withDefault (Just c) = c
withDefault Nothing = undefined

toPairs :: String -> (Char, Char)
toPairs cx =
  (withDefault . findFirstDigit $ cx,
   withDefault . findFirstDigit . reverse $ cx)

join :: (Char, Char) -> String
join (a, b) = [a, b]

toInt :: String -> Integer
toInt x = read x :: Integer

main = do
  values <- readFile "values.txt"

  print . foldr (+) 0 $ map (toInt . join . toPairs) . lines $ values
