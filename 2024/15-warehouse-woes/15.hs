{-# LANGUAGE QuasiQuotes #-}

import           Data.Array
import           Data.List         (find)
import           Data.List.Split   (chunksOf, splitOn)
import           Text.RawString.QQ (r)

main = readFile "in" >>= print . process

test = tail [r|
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
|]

test' = tail [r|
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
|]

process input = score $ fst $ foldl (\(grid, pos) move -> moveBot grid pos move) (grid, pos) moves
  where [gridStr, movesStr] = splitOn "\n\n" input
        grid = parseGrid gridStr
        pos = botPosition gridStr
        moves = [parseMove move | move <- movesStr, move /= '\n']

moveBot grid pos direction = case moveTo of
  Empty -> (grid, nextPos)
  Wall -> (grid, pos)
  Box -> case shiftBoxes [] grid nextPos direction of
    Just shifted -> (shifted, nextPos)
    Nothing      -> (grid, pos)
  where nextPos = offset direction ^+^ pos
        moveTo = grid ! nextPos

shiftBoxes :: [((Int, Int), Cell)] -> Array (Int, Int) Cell -> (Int, Int) -> Move -> Maybe (Array (Int, Int) Cell)
shiftBoxes acc grid pos direction = case moveTo of
  Empty -> Just $ grid // shifts
  Box   -> shiftBoxes shifts grid nextPos direction
  Wall  -> Nothing
  where nextPos = offset direction ^+^ pos
        moveTo = grid ! nextPos
        shifts = (pos, Empty) : (nextPos, Box) : acc

score grid = sum [(y - 1) * 100 + x - 1 | ((y, x), cell) <- assocs grid, cell == Box]

display grid bot = unlines $ chunksOf w $ concat $ elems $ (// [(bot, "@")]) $ fmap show grid
  where ((1, 1), (h, w)) = bounds grid

offset U = (-1, 0)
offset D = (1, 0)
offset L = (0, -1)
offset R = (0, 1)

(a, b) ^+^ (c, d) = (a + c, b + d)

data Cell = Empty | Wall | Box deriving Eq

parseCell '.' = Empty
parseCell '#' = Wall
parseCell 'O' = Box
parseCell '@' = Empty

parseGrid :: String -> Array (Int, Int) Cell
parseGrid input = listArray ((1, 1), (h, w)) $ map parseCell $ concat $ lines input
  where h = length $ lines input
        w = length $ head $ lines input

botPosition :: String -> (Int, Int)
botPosition input = pos
  where indexed = zip (range ((1, 1), (h, w))) $ concat $ lines input
        (h, w) = (length $ lines input, length $ head $ lines input)
        Just (pos, _) = find (\(_, c) -> c == '@') indexed


instance Show Cell where
  show Empty = "."
  show Wall  = "#"
  show Box   = "O"

data Move = L | R | U | D

parseMove 'v' = D
parseMove '^' = U
parseMove '<' = L
parseMove '>' = R

instance Show Move where
  show L = "<"
  show R = ">"
  show U = "^"
  show D = "v"
