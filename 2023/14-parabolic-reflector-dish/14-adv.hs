import           Data.Array      (assocs, bounds, elems, listArray, (!), (//))
import           Data.List.Split (chunksOf)
import           Data.Map        (Map)
import qualified Data.Map        as Map

main = readFile "in" >>= print . process

test = "\
\O....#....\n\
\O.OO#....#\n\
\.....##...\n\
\OO.#O....O\n\
\.O.....O#.\n\
\O.#..O.#.#\n\
\..O..#O..O\n\
\.......O..\n\
\#....###..\n\
\#OO..#...."

shiftedNorth = "\
\OOOO.#.O..\n\
\OO..#....#\n\
\OO..O##..O\n\
\O..#.OO...\n\
\........#.\n\
\..#....#.#\n\
\..O..#.O.O\n\
\..O.......\n\
\#....###..\n\
\#....#...."

cycle1 = "\
\.....#....\n\
\....#...O#\n\
\...OO##...\n\
\.OO#......\n\
\.....OOO#.\n\
\.O#...O#.#\n\
\....O#....\n\
\......OOOO\n\
\#...O###..\n\
\#..OO#...."

cycle3 = "\
\.....#....\n\
\....#...O#\n\
\.....##...\n\
\..O#......\n\
\.....OOO#.\n\
\.O#...O#.#\n\
\....O#...O\n\
\.......OOO\n\
\#...O###.O\n\
\#.OOO#...O"

target = 1_000_000_000

process input = take 200 $ map score cycles
    where cycles = iterate shiftCycle $ parseGrid input
          (cycleStart, cycleLength) = findCycle Map.empty $ zip [1..] cycles
          idxIntoCycle = cycleStart + ((target - cycleStart) `mod` cycleLength) - 1

findCycle visited ((idx, grid):rest) = case Map.lookup grid visited of
    Just prevIdx -> (prevIdx, idx - prevIdx)
    Nothing      -> findCycle (Map.insert grid idx visited) rest


data Tile = Empty | RoundRock | SquareRock deriving (Eq, Ord)

instance Show Tile where
    show Empty      = "."
    show RoundRock  = "O"
    show SquareRock = "#"

parseGrid input = listArray ((1,1), (width, height)) $ map parseTile $ filter (/= '\n') input
    where width = length $ head $ lines input
          height = length $ lines input

score' anchor []                       = 0
score' anchor ((y, Empty):column)      = score' anchor column
score' anchor ((y, RoundRock):column)  = anchor + score' (anchor - 1) column
score' anchor ((y, SquareRock):column) = score' (y - 1) column

colScore column = score' (length column) column

score = sum . map colScore . columns

columns grid = [ [(y, grid ! (y, x)) | y <- [1..height] ] | x <- [1..width] ]
    where ((1,1), (width, height)) = bounds grid
rows grid = [ [(x, grid ! (y, x)) | x <- [1..width] ] | y <- [1..height] ]
    where ((1,1), (width, height)) = bounds grid

parseTile '.' = Empty
parseTile 'O' = RoundRock
parseTile '#' = SquareRock

shiftNorth grid = rocklessGrid // [(coord, RoundRock) | coord <- roundRockPositions]
    where rocklessGrid = grid // [(coord, Empty) | (coord, tile) <- assocs grid, tile == RoundRock]
          roundRocksInCols = map shiftColNorth $ columns grid
          roundRockPositions = concat $ zipWith (\x ys -> map (, x) ys) [1..] roundRocksInCols

shiftColNorth = shiftColNorth' [] Nothing
shiftColNorth' acc Nothing  ((y, Empty     ):column) = shiftColNorth' acc (Just y) column
shiftColNorth' acc (Just x) ((y, Empty     ):column) = shiftColNorth' acc (Just x) column
shiftColNorth' acc _        ((_, SquareRock):column) = shiftColNorth' acc Nothing column
shiftColNorth' acc Nothing  ((y, RoundRock ):column) = shiftColNorth' (y:acc) Nothing column
shiftColNorth' acc (Just x) ((y, RoundRock ):column) = shiftColNorth' (x:acc) (Just $ x + 1) column
shiftColNorth' acc _ [] = acc

showGrid grid = unlines $ chunksOf width $ concatMap show $ elems grid
    where (_, (width, _)) = bounds grid

shiftColSouth = shiftColSouth' [] Nothing . reverse
shiftColSouth' acc Nothing  ((y, Empty     ):column) = shiftColSouth' acc (Just y) column
shiftColSouth' acc (Just x) ((y, Empty     ):column) = shiftColSouth' acc (Just x) column
shiftColSouth' acc _        ((_, SquareRock):column) = shiftColSouth' acc Nothing column
shiftColSouth' acc Nothing  ((y, RoundRock ):column) = shiftColSouth' (y:acc) Nothing column
shiftColSouth' acc (Just x) ((y, RoundRock ):column) = shiftColSouth' (x:acc) (Just $ x - 1) column
shiftColSouth' acc _ [] = acc

shiftRowEast = shiftColSouth' [] Nothing . reverse
shiftRowEast' acc Nothing     ((x, Empty     ):column) = shiftRowEast' acc        (Just x)          column
shiftRowEast' acc (Just prev) ((_, Empty     ):column) = shiftRowEast' acc        (Just prev)       column
shiftRowEast' acc _           ((_, SquareRock):column) = shiftRowEast' acc        Nothing           column
shiftRowEast' acc Nothing     ((x, RoundRock ):column) = shiftRowEast' (x:acc)    Nothing           column
shiftRowEast' acc (Just prev) ((x, RoundRock ):column) = shiftRowEast' (prev:acc) (Just $ prev - 1) column
shiftRowEast' acc _ [] = acc

shiftRowWest = shiftRowWest' [] Nothing
shiftRowWest' acc Nothing     ((x, Empty     ):column) = shiftRowWest' acc        (Just x)          column
shiftRowWest' acc (Just prev) ((x, Empty     ):column) = shiftRowWest' acc        (Just prev)       column
shiftRowWest' acc _           ((_, SquareRock):column) = shiftRowWest' acc        Nothing           column
shiftRowWest' acc Nothing     ((x, RoundRock ):column) = shiftRowWest' (x:acc)    Nothing           column
shiftRowWest' acc (Just prev) ((x, RoundRock ):column) = shiftRowWest' (prev:acc) (Just $ prev + 1) column
shiftRowWest' acc _ [] = acc


data Direction = North | East | South | West

newPositionShifted North = concat . zipWith (\x ys -> map (, x) ys) [1..] . map shiftColNorth . columns
newPositionShifted South = concat . zipWith (\x ys -> map (, x) ys) [1..] . map shiftColSouth . columns
newPositionShifted East  = concat . zipWith (\y xs -> map (y, ) xs) [1..] . map shiftRowEast . rows
newPositionShifted West  = concat . zipWith (\y xs -> map (y, ) xs) [1..] . map shiftRowWest . rows

shift direction grid = rocklessGrid // map (, RoundRock) (newPositionShifted direction grid)
    where rocklessGrid = grid // [(coord, Empty) | (coord, tile) <- assocs grid, tile == RoundRock]

shiftCycle grid = foldl (flip shift) grid [North, West, South, East]
