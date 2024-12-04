import           Data.Array

main = readFile "in" >>= print . process

test2 = "\
\..X...\n\ 
\.SAMX.\n\
\.A..A.\n\
\XMAS.S\n\
\.X....\n"

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

process input = sum $ map (countTowards grid) directions
  where grid = listArray ((1,1), (y, x)) $ filter (/= '\n') input
        x = length $ takeWhile (/= '\n') input
        y = length (filter (== '\n') input)

countTowards grid direction = length $ filter (explore grid "XMAS" direction) $ seeds grid

seeds = map fst . filter ((== 'X') . snd) . assocs

explore grid (expectedChar:tail) (dy, dx) (y, x)
  | not $ inRange (bounds grid) (y, x) = False
  | grid ! (y, x) == expectedChar = explore grid tail (dy, dx) (y+dy, x+dx)
  | otherwise = False

explore _ [] _ _ = True

directions = 
  [ (-1, -1), (-1, 0), (-1, 1)
  , (0 , -1),          (0 , 1)
  , (1 , -1), (1 , 0), (1 , 1)]
