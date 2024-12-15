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

test'' = tail [r|
#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^
|]

process input = score $ fst $ foldl (\(grid, pos) move -> moveBot grid pos move) (grid, pos) moves
  where [gridStr, movesStr] = splitOn "\n\n" input
        grid = parseGrid gridStr
        pos = botPosition gridStr
        moves = [parseMove move | move <- movesStr, move /= '\n']

moveBot grid pos direction = case moveTo of
  Empty -> (grid, nextPos)
  Wall -> (grid, pos)
  _ -> case shiftBoxes [] grid nextPos direction of
    Just shifted -> (shifted, nextPos)
    Nothing      -> (grid, pos)
  where nextPos = offset direction ^+^ pos
        moveTo = grid ! nextPos

shiftBoxes :: [((Int, Int), Cell)] -> Array (Int, Int) Cell -> (Int, Int) -> Move -> Maybe (Array (Int, Int) Cell)

shiftBoxes acc grid pos L = case grid ! afterPos of
  Empty    -> Just $ grid // shifts
  Wall     -> Nothing
  RightBox -> shiftBoxes shifts grid afterPos L
  where nextPos = pos ^+^ (0, -1)
        afterPos = nextPos ^+^ (0, -1)
        shifts = (pos, Empty) : (nextPos, RightBox) : (afterPos, LeftBox) : acc

shiftBoxes acc grid pos R = case grid ! afterPos of
  Empty   -> Just $ grid // shifts
  Wall    -> Nothing
  LeftBox -> shiftBoxes shifts grid afterPos R
  where nextPos = pos ^+^ (0, 1)
        afterPos = nextPos ^+^ (0, 1)
        shifts = (pos, Empty) : (nextPos, LeftBox) : (afterPos, RightBox) : acc

shiftBoxes acc grid pos U = case (grid ! lNextPos, grid ! rNextPos) of
  (Empty, Empty) -> Just $ grid // shifts
  (Wall, _) -> Nothing
  (_, Wall) -> Nothing
  (LeftBox, RightBox) -> shiftBoxes shifts grid lNextPos U
  (RightBox, Empty) -> shiftBoxes shifts grid lNextPos U
  (Empty, LeftBox) -> shiftBoxes shifts grid rNextPos U
  (RightBox, LeftBox) -> do
    leftShiftedGrid <- shiftBoxes [] grid lNextPos U
    shiftBoxes shifts leftShiftedGrid rNextPos U
  where (lNextPos, rNextPos) = case grid ! pos of
          LeftBox  -> (pos ^+^ (-1, 0), pos ^+^ (-1, 1))
          RightBox -> (pos ^+^ (-1, -1), pos ^+^ (-1, 0))
        shifts = case grid ! pos of
          LeftBox ->
              (pos, Empty)
            : (pos ^+^ (0, 1), Empty)
            : (pos ^+^ (-1, 0), LeftBox)
            : (pos ^+^ (-1, 1), RightBox)
            : acc
          RightBox ->
              (pos, Empty)
            : (pos ^+^ (0, -1), Empty)
            : (pos ^+^ (-1, 0), RightBox)
            : (pos ^+^ (-1, -1), LeftBox)
            : acc

shiftBoxes acc grid pos D = case (grid ! lNextPos, grid ! rNextPos) of
  (Empty, Empty) -> Just $ grid // shifts
  (Wall, _) -> Nothing
  (_, Wall) -> Nothing
  (LeftBox, RightBox) -> shiftBoxes shifts grid lNextPos D
  (RightBox, Empty) -> shiftBoxes shifts grid lNextPos D
  (Empty, LeftBox) -> shiftBoxes shifts grid rNextPos D
  (RightBox, LeftBox) -> do
    leftShiftedGrid <- shiftBoxes [] grid lNextPos D
    shiftBoxes shifts leftShiftedGrid rNextPos D
  where (lNextPos, rNextPos) = case grid ! pos of
          LeftBox  -> (pos ^+^ (1, 0), pos ^+^ (1, 1))
          RightBox -> (pos ^+^ (1, -1), pos ^+^ (1, 0))
        shifts = case grid ! pos of
          LeftBox ->
              (pos, Empty)
            : (pos ^+^ (0, 1), Empty)
            : (pos ^+^ (1, 0), LeftBox)
            : (pos ^+^ (1, 1), RightBox)
            : acc
          RightBox ->
              (pos, Empty)
            : (pos ^+^ (0, -1), Empty)
            : (pos ^+^ (1, 0), RightBox)
            : (pos ^+^ (1, -1), LeftBox)
            : acc

score grid = sum [(y - 1) * 100 + x - 1 | ((y, x), cell) <- assocs grid, cell == LeftBox]

display grid bot = unlines $ chunksOf w $ concat $ elems $ (// [(bot, "@")]) $ fmap show grid
  where ((1, 1), (h, w)) = bounds grid

(a, b) ^+^ (c, d) = (a + c, b + d)

data Cell = Empty | Wall | LeftBox | RightBox deriving Eq

parseCell '.' = [Empty, Empty]
parseCell '#' = [Wall, Wall]
parseCell 'O' = [LeftBox, RightBox]
parseCell '@' = [Empty, Empty]

parseGrid :: String -> Array (Int, Int) Cell
parseGrid input = listArray ((1, 1), (h, w * 2)) $ concatMap parseCell $ concat $ lines input
  where h = length $ lines input
        w = length $ head $ lines input

botPosition :: String -> (Int, Int)
botPosition input = (y, x * 2 - 1)
  where indexed = zip (range ((1, 1), (h, w))) $ concat $ lines input
        (h, w) = (length $ lines input, length $ head $ lines input)
        Just ((y, x), _) = find (\(_, c) -> c == '@') indexed


instance Show Cell where
  show Empty    = "."
  show Wall     = "#"
  show LeftBox  = "["
  show RightBox = "]"

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
