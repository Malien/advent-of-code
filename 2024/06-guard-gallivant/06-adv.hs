import           Data.Array (Ix (range))
import           Data.List  (find)
import           Data.Set   (Set)
import qualified Data.Set   as Set

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

process input = length $ filter hasCycleWithNewObstacle additionalObstacles
  where (bounds, obstacles, start) = parseGrid input
        hasCycleWithNewObstacle obstacle = hasCycle Set.empty (Set.insert obstacle obstacles) bounds start North
        firstWalk = walkPath Set.empty obstacles bounds start North
        additionalObstacles
          = filter (`Set.member` firstWalk) 
          $ filter (`Set.notMember` obstacles) 
          $ filter (/= start) 
          $ range ((1, 1), bounds)

data Direction = North | East | South | West deriving (Show, Eq, Ord)

move (y, x) North = (y-1, x)
move (y, x) East  = (y, x+1)
move (y, x) South = (y+1, x)
move (y, x) West  = (y, x-1)

turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

hasCycle visited obstacles bounds position direction
  | not (withinBounds bounds position) = False
  | (direction, position) `Set.member` visited = True
  | nextPosition `Set.member` obstacles = hasCycle nextVisisted obstacles bounds position nextDirection
  | otherwise = hasCycle nextVisisted obstacles bounds nextPosition direction
  where nextPosition = move position direction
        nextVisisted = Set.insert (direction, position) visited
        nextDirection = turnRight direction

walkPath visited obstacles bounds position direction
  | not (withinBounds bounds position) = visited
  | nextPosition `Set.member` obstacles = walkPath nextVisisted obstacles bounds position nextDirection
  | otherwise = walkPath nextVisisted obstacles bounds nextPosition direction
  where nextPosition = move position direction
        nextVisisted = Set.insert position visited
        nextDirection = turnRight direction


withinBounds (maxY, maxX) (y, x) = y >= 1 && x >= 1 && y <= maxY && x <= maxX

parseGrid input = (bounds, obstacles, startingPoint)
  where bounds = (length $ head $ lines input, length $ lines input)
        withPositions = zip (range ((1, 1), bounds)) (filter (/= '\n') input)
        obstacles = Set.fromList [pos | (pos, char) <- withPositions, char == '#']
        (Just startingPoint) = fst <$> find ((== '^') . snd) withPositions


combineMaybe (Just a) _ = Just a
combineMaybe _ (Just a) = Just a
combineMaybe _ _        = Nothing
