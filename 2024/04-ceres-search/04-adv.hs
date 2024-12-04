import           Data.Array

main = readFile "in" >>= print . process

test = "\
\MMMSXXMASM\n\
\MSAMXMSMSA\n\
\AMXSXMAAMM\n\
\MSAMASMSMX\n\
\XMASAMXAMM\n\
\XXAMMXXAMA\n\
\SMSMSASXSS\n\
\SAXAMASAAA\n\
\MAMMMXMMMM\n\
\MXMXAXMASX\n"

process input = length $ filter (hasCross grid) $ seeds grid
  where grid = listArray ((1,1), (y, x)) $ filter (/= '\n') input
        x = length $ takeWhile (/= '\n') input
        y = length (filter (== '\n') input)

seeds :: Ix i => Array i Char -> [i]
seeds = map fst . filter ((== 'A') . snd) . assocs

expect grid index char = inRange (bounds grid) index && grid ! index == char

hasCross grid (y, x) =
  ((expect grid (y-1, x-1) 'M' && expect grid (y+1, x+1) 'S') ||
  (expect grid (y-1, x-1) 'S' && expect grid (y+1, x+1) 'M')) &&
  ((expect grid (y-1, x+1) 'M' && expect grid (y+1, x-1) 'S') ||
  (expect grid (y-1, x+1) 'S' && expect grid (y+1, x-1) 'M'))
