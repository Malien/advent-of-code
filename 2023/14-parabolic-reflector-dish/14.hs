import           Data.Array (bounds, listArray, (!))

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

process = sum . map score . columns . parseGrid

data Tile = Empty | RoundRock | SquareRock deriving Eq

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

score :: [Tile] -> Int
score column = score' (length column) $ zip [length column, length column - 1..1] column

columns grid = [ [ grid ! (y, x) | y <- [1..height] ] | x <- [1..width] ]
    where ((1,1), (width, height)) = bounds grid

parseTile '.' = Empty
parseTile 'O' = RoundRock
parseTile '#' = SquareRock
