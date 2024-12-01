import           Data.List (sort)

main = readFile "in" >>= print . process

test = "\
\3   4\n\
\4   3\n\
\2   5\n\
\1   3\n\
\3   9\n\
\3   3"


process input = sum $ zipWith distance (sort left) (sort right)
  where pairs = parse input
        left = map head pairs
        right = map (!! 1) pairs

parse :: String -> [[Int]]
parse = map (map read . words) . lines

distance a b = abs (a - b)
