main = readFile "in" >>= print . process

test = "\
\0 3 6 9 12 15\n\
\1 3 6 10 15 21\n\
\10 13 16 21 30 45"

process = sum . map (predict . diffChain) . parse

parse = map (map (read :: String -> Int) . words) . lines

diffs (x:y:xs) = y - x : diffs (y:xs)
diffs _ = []

diffChain = takeWhile (not . all (==0)) . iterate diffs

fillIn chain prev = head chain - prev

predict = foldr fillIn 0
