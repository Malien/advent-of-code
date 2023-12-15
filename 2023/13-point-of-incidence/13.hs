import           Data.Array      (bounds, listArray, (!))
import           Data.List       (find)
import           Data.List.Split (splitOn)

main = readFile "in" >>= print . process

test = "\
\#.##..##.\n\
\..#.##.#.\n\
\##......#\n\
\##......#\n\
\..#.##.#.\n\
\..##..##.\n\
\#.#.##.#.\n\
\\n\
\#...##..#\n\
\#....#..#\n\
\..##..###\n\
\#####.##.\n\
\#####.##.\n\
\..##..###\n\
\#....#..#"

process = sum . map (uncurry score . splits . parseGrid) . splitOn "\n\n"

splits grid = (horizSplitPoint grid, vertSplitPoint grid)

score (Just x) Nothing = x
score Nothing (Just y) = y * 100

data El = Ash | Rock deriving Eq
instance Show El where
  show Ash  = "#"
  show Rock = "."

parseElement '#' = Ash
parseElement '.' = Rock

parseGrid input = listArray ((1,1),(height,width)) $ map parseElement $ filter (/= '\n') input
    where height = length $ lines input
          width  = length $ head $ lines input

reflectedCols grid col
    | col <= width `div` 2 = ([1..col], [col+1..col+col])
    | otherwise            = ([col + col - width + 1 .. col], [col + 1 .. width])
    where (_, (_, width))  = bounds grid

isHorizReflected grid col =
      all (\(source, target) -> grid ! source == grid ! target)
    $ zip (coords left) (coords $ reverse right)
    where (left, right) = reflectedCols grid col
          ((minY, _), (maxY, width)) = bounds grid
          coords cols = [(y, x) | y <- [minY..maxY], x <- cols]

horizSplitPoint grid = find (isHorizReflected grid) [1..width - 1]
    where (_, (_, width)) = bounds grid

reflectedRows grid row
    | row <= height `div` 2 = ([1..row], [row+1..row+row])
    | otherwise             = ([row + row - height + 1 .. row], [row + 1 .. height])
    where (_, (height, _))  = bounds grid

isVertReflected grid row =
      all (\(source, target) -> grid ! source == grid ! target)
    $ zip (coords top) (coords $ reverse bottom)
    where (top, bottom) = reflectedRows grid row
          ((_, minX), (_, maxX)) = bounds grid
          coords rows = [(y, x) | y <- rows, x <- [minX..maxX]]

vertSplitPoint grid = find (isVertReflected grid) [1..height - 1]
    where (_, (height, _)) = bounds grid
