main = readFile "in" >>= print . process

test = "\
\9 2 3 4 5\n\
\1 9 3 4 5\n\
\9 4 3 2 1\n\
\4 9 3 2 1\n\
\1 2 3 4 9\n\
\1 2 3 9 4\n\
\5 4 3 2 9\n\
\5 4 3 9 2\n\
\41 38 40 42 44 47\n\
\12 15 9 7 6 5\n\
\"

test2 = "\
\7 6 4 2 1\n\
\1 2 7 8 9\n\
\9 7 6 2 1\n\
\1 3 2 4 5\n\
\8 6 4 4 1\n\
\1 3 6 7 9\n\
\"

process = length . filter id . map (lenientSlope Unknown []) . parse

-- process input = map fst $ filter (not . snd) $ zip rows $ map (lenientSlope Unknown []) rows
--   where rows = parse input

parse :: String -> [[Int]]
parse = map (map read . words) . lines

data Slope = Increasing | Decreasing | Unknown deriving (Show)

lenientSlope :: Slope -> [(Slope, [Int])] -> [Int] -> Bool
lenientSlope Unknown fallbacks (x:y:xs)
  | x == y = any (uncurry safeSlope) fallbacks
  | x < y && y - x <= 3 = lenientSlope Increasing [(Unknown, x:xs), (Unknown, y:xs)] (y:xs)
  | x > y && x - y <= 3 = lenientSlope Decreasing [(Unknown, x:xs), (Unknown, y:xs)] (y:xs)
  | otherwise = any (uncurry safeSlope) fallbacks

lenientSlope Increasing fallbacks (x:y:xs)
  | x == y = any (uncurry safeSlope) fallbacks
  | y < x = any (uncurry safeSlope) fallbacks
  | y - x <= 3 = lenientSlope Increasing ((Increasing, x:xs):fallbacks) (y:xs)
  | otherwise = any (uncurry safeSlope) fallbacks

lenientSlope Decreasing fallbacks (x:y:xs)
  | x == y = any (uncurry safeSlope) fallbacks
  | y > x = any (uncurry safeSlope) fallbacks
  | x - y <= 3 = lenientSlope Decreasing ((Decreasing, x:xs):fallbacks) (y:xs)
  | otherwise = any (uncurry safeSlope) fallbacks

lenientSlope _ _ _ = True


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

