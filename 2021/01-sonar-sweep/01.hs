main = parseProblem >>= print . solution . map sum . windowed 3

zipWithNext f l = zipWith f l (tail l)

solution = length . filter id . zipWithNext (<)

windowed size xs = scanl (\window x -> (tail window) ++ [x]) (take size xs) (drop size xs)

parseProblem = map read . words <$> readFile "input.txt"















chunked _ [] = []
chunked size xs = take size xs : chunked size (drop size xs)

