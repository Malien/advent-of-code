main = solution . map sum . windowed 3 <$> parseProblem

zipNext l = zip l (tail l)

solution = length . filter (uncurry (<)) . zipNext

windowed size xs = scanl (\window x -> (tail window) ++ [x]) (take size xs) (drop size xs)

parseProblem = map read . words <$> readFile "input.txt"















chunked _ [] = []
chunked size xs = take size xs : chunked size (drop size xs)

