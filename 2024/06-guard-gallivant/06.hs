import           Data.Set        (Set)
import qualified Data.Set        as Set

main = readFile "in" >>= print . process

test = "\
\....#.....\n\
\.........#\n\
\..........\n\
\..#.......\n\
\.......#..\n\
\..........\n\
\.#..^.....\n\
\........#.\n\
\#.........\n\
\......#...\n\
\"

process input = Set.size $ walk Set.empty obstacles bounds start North
  where (bounds, obstacles, start) = parseGrid input

data Direction = North | East | South | West deriving (Show, Eq)

move (y, x) North = (y-1, x)
move (y, x) East = (y, x+1)
move (y, x) South = (y+1, x)
move (y, x) West = (y, x-1)

turnRight North = East
turnRight East = South 
turnRight South = West
turnRight West = North

walk visited obstacles bounds position direction 
  | not (withinBounds bounds position) = visited
  | nextPosition `Set.member` obstacles = walk nextVisisted obstacles bounds position nextDirection
  | otherwise = walk nextVisisted obstacles bounds nextPosition direction
  where nextPosition = move position direction
        nextVisisted = Set.insert position visited
        nextDirection = turnRight direction

withinBounds (maxY, maxX) (y, x) = y >= 0 && x >= 0 && y < maxY && x < maxX

parseGrid input = (bounds, obstacles, startingPoint)
  where indexed = zip [0..] (map (zip [0..]) $ lines input)
        obstacles = foldr1 Set.union $ map (uncurry (parseRow Set.empty)) indexed
        (Just startingPoint) = foldr1 combineMaybe $ map (uncurry findStartInRow) indexed
        bounds = (length $ head $ lines input, length $ lines input)

parseRow acc y ((x,'.'):tail) = parseRow acc y tail
parseRow acc y ((x,'^'):tail) = parseRow acc y tail
parseRow acc y ((x,'#'):tail) = parseRow (Set.insert (y, x) acc) y tail
parseRow acc _ [] = acc

findStartInRow y ((x,'^'):tail) = Just (y, x)
findStartInRow y (_:tail) = findStartInRow y tail
findStartInRow _ [] = Nothing

combineMaybe (Just a) _ = Just a
combineMaybe _ (Just a) = Just a
combineMaybe _ _ = Nothing

