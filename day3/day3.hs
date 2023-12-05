data Coordinate x y = Coordinate { x :: x, y :: y }
  deriving (Show)
data TextBoundary a b c = TextBoundary { start :: a, end :: b, text :: String }
  deriving (Show)

createCoordinate :: x -> y -> Coordinate x y
createCoordinate x y = Coordinate x y

-- Collect numbers until the next non-numeric value 
collectNumbers :: String -> [(Integer, Char)] -> (String, [(Integer, Char)])
collectNumbers c [] = (c, [])
collectNumbers c ((y, s):sx)
  | s `elem` "0123456789" = collectNumbers (c ++ [s]) sx
  | otherwise = (c, (y, s):sx)

reduceNumbers :: [(Integer, String)] -> [(Integer, Char)] -> [(Integer, String)]
reduceNumbers c [] = c
reduceNumbers c ((y, v):sx) =
  if v `elem` "0123456789" then do
    let (restNumber, sx') = collectNumbers "" sx
    reduceNumbers (c ++ [(y, v:restNumber)]) sx'
  else
    reduceNumbers c sx

toBoundary :: Integer -> Integer -> String -> TextBoundary (Coordinate Integer Integer) (Coordinate Integer Integer) String
toBoundary x y v = TextBoundary (Coordinate (x-1) (y-1)) (Coordinate (x+1) (y+(toInteger $ length v))) v

main = do
  values <- readFile "./values.txt"

  let valueLines = lines values
  let matrixValues = zipWith (\x b -> (x, zipWith (\y v -> (Coordinate x y, v)) [0 :: Int ..] b)) [0 :: Int ..] $ valueLines
  let symbolCoordinates = (map (\(a, b) -> (a, filter (\(_, v) -> v `elem` "!@#$%^&*()_+-=") b)) $ matrixValues) >>= \(r, l) -> l
  let rowsWithIndexes = zipWith (\x r -> (x, r)) [0 :: Integer ..] valueLines
  let matrixText = map(\(x, r) -> (x, reduceNumbers [] $ zipWith (\y c -> (y, c)) [0 :: Integer ..] r)) $ rowsWithIndexes
  let textBoundaries = (map (\(x, r) -> map (\(y, v) -> toBoundary x y v) r) $ matrixText) >>= \l -> l


  print $ textBoundaries
