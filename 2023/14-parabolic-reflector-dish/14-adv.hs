{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
import           Data.Array      (Array, Ix, assocs, bounds, elems, listArray,
                                  (!), (//))
import           Data.Hashable
import           Data.List.Split (chunksOf)
import qualified Data.Map        as Map
import           GHC.Generics    (Generic)

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

process input = score $ cycles !! idxIntoCycle
    where cycles = iterate shiftCycle $ parseGrid input
          (cycleStart, cycleLength) = findCycle Map.empty $ zip [1..] cycles
          idxIntoCycle = cycleStart + ((target - cycleStart) `mod` cycleLength)

findCycle visited ((idx, grid):rest) = case Map.lookup (hash grid) visited of
    Just prevIdx -> (prevIdx, idx - prevIdx)
    Nothing      -> findCycle (Map.insert (hash grid) idx visited) rest


data Tile = Empty | RoundRock | SquareRock deriving (Eq, Ord, Generic)

instance Hashable Tile

instance (Hashable k, Hashable v, Ix k) => Hashable (Array k v) where
    hashWithSalt salt arr = hashWithSalt salt $ elems arr

instance Show Tile where
    show Empty      = "."
    show RoundRock  = "O"
    show SquareRock = "#"

parseGrid input = listArray ((1,1), (width, height)) $ map parseTile $ filter (/= '\n') input
    where width = length $ head $ lines input
          height = length $ lines input

score grid = 
    sum $
    map fst $
    filter ((== RoundRock) . snd) $
    concatMap (zip [1..] . reverse) $
    [ [grid ! (y, x) | y <- [1..height] ] | x <- [1..width] ]
    where ((1, 1), (width, height)) = bounds grid


columns grid = [ [(y, grid ! (y, x)) | y <- [1..height] ] | x <- [1..width] ]
    where ((1,1), (width, height)) = bounds grid
rows grid = [ [(x, grid ! (y, x)) | x <- [1..width] ] | y <- [1..height] ]
    where ((1,1), (width, height)) = bounds grid

parseTile '.' = Empty
parseTile 'O' = RoundRock
parseTile '#' = SquareRock

showGrid grid = unlines $ chunksOf width $ concatMap show $ elems grid
    where (_, (width, _)) = bounds grid

shiftNeg = shiftNeg' [] Nothing . reverse
shiftNeg' acc Nothing     ((x, Empty     ):column) = shiftNeg' acc        (Just x)          column
shiftNeg' acc (Just prev) ((_, Empty     ):column) = shiftNeg' acc        (Just prev)       column
shiftNeg' acc _           ((_, SquareRock):column) = shiftNeg' acc        Nothing           column
shiftNeg' acc Nothing     ((x, RoundRock ):column) = shiftNeg' (x:acc)    Nothing           column
shiftNeg' acc (Just prev) ((_, RoundRock ):column) = shiftNeg' (prev:acc) (Just $ prev - 1) column
shiftNeg' acc _ [] = acc

shiftPos = shiftPos' [] Nothing
shiftPos' acc Nothing     ((x, Empty     ):xs) = shiftPos' acc        (Just x)          xs
shiftPos' acc (Just prev) ((_, Empty     ):xs) = shiftPos' acc        (Just prev)       xs
shiftPos' acc _           ((_, SquareRock):xs) = shiftPos' acc        Nothing           xs
shiftPos' acc Nothing     ((x, RoundRock ):xs) = shiftPos' (x:acc)    Nothing           xs
shiftPos' acc (Just prev) ((_, RoundRock ):xs) = shiftPos' (prev:acc) (Just $ prev + 1) xs
shiftPos' acc _ [] = acc

data Direction = North | East | South | West

newPositionShifted North = concat . zipWith (\x ys -> map (, x) ys) [1..] . map shiftPos . columns
newPositionShifted South = concat . zipWith (\x ys -> map (, x) ys) [1..] . map shiftNeg . columns
newPositionShifted East  = concat . zipWith (\y xs -> map (y, ) xs) [1..] . map shiftNeg . rows
newPositionShifted West  = concat . zipWith (\y xs -> map (y, ) xs) [1..] . map shiftPos . rows

shift direction grid = rocklessGrid // map (, RoundRock) (newPositionShifted direction grid)
    where rocklessGrid = grid // [(coord, Empty) | (coord, tile) <- assocs grid, tile == RoundRock]

shiftCycle grid = foldl (flip shift) grid [North, West, South, East]
