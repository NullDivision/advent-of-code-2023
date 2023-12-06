main = do
  values <- readFile "values.txt"

  print . foldr (+) 0 . map (\(w, n) ->
    let c = length $ filter (\i -> i `elem` w) n
    in if c == 0 then 0 else 2^(c-1)) . map (\l ->
    let (w, n) = break (\c -> c == '|') $ drop 10 l
    in (map (\w -> read w :: Integer) $ words w, map (\w -> read w :: Integer) . words $ drop 1 n)) . lines $ values
