main = readFile "in" >>= print . process

test = "\
\7 6 4 2 1\n\
\1 2 7 8 9\n\
\9 7 6 2 1\n\
\1 3 2 4 5\n\
\8 6 4 4 1\n\
\1 3 6 7 9\n\
\"

process = length . filter id . map (safeSlope Unknown) . parse

parse :: String -> [[Int]]
parse = map (map read . words) . lines

data Slope = Increasing | Decreasing | Unknown

safeSlope Unknown (x:y:xs) 
  | x == y = False
  | x < y && y - x <= 3 = safeSlope Increasing (y:xs)
  | x > y && x - y <= 3 = safeSlope Decreasing (y:xs)
  | otherwise = False

safeSlope Increasing (x:y:xs)
  | x == y = False
  | y < x = False
  | y - x <= 3 = safeSlope Increasing (y:xs)
  | otherwise = False

safeSlope Decreasing (x:y:xs)
  | x == y = False
  | y > x = False
  | x - y <= 3 = safeSlope Decreasing (y:xs)
  | otherwise = False

safeSlope _ _ = True

