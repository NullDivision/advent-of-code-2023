import Prelude
import Text.Regex.TDFA
import Debug.Trace

tokenRegex = "([1-9]|one|two|three|four|five|six|seven|eight|nine)" 

findToken :: String -> (String, String)
findToken s =
  let (before, token, after) = s =~ tokenRegex :: (String, String, String)
  in (token, after)

unwrapToken :: Maybe String -> String
unwrapToken (Just x) = x
unwrapToken Nothing = ""

expandToken :: [Char] -> Int -> Maybe String
expandToken x d = do
  let removableLength = (length x) - d
  let remainingText = drop removableLength x
  let (token, after) = findToken remainingText

  if token == "" && remainingText /= x
    then expandToken x (d+1)
    else if token == ""
      then Nothing
      else Just token

toInt :: String -> String
toInt x = case x of
               "one"   -> "1"
               "two"   -> "2"
               "three" -> "3"
               "four"  -> "4"
               "five"  -> "5"
               "six"   -> "6"
               "seven" -> "7"
               "eight" -> "8"
               "nine"  -> "9"
               _       -> x

findTokens :: String -> Integer
findTokens s = do
  let (token, rest) = findToken $ s
  let lastToken = unwrapToken (expandToken s 1);

  read ((toInt $ token) ++ (toInt $ lastToken)) :: Integer

main = do
  values <- readFile "values.txt"

  print . foldr (+) 0 . map(findTokens) . lines $ values
