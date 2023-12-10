import           Data.Array (Array, assocs, bounds, listArray, (!), (//))
import           Data.List  (find)
import           Data.Set   (Set)
import qualified Data.Set   as Set
import           Prelude

main = readFile "in" >>= print . process

test1 = "\
\...........\n\
\.S-------7.\n\
\.|F-----7|.\n\
\.||.....||.\n\
\.||.....||.\n\
\.|L-7.F-J|.\n\
\.|..|.|..|.\n\
\.L--J.L--J.\n\
\..........."

test2 = "\
\.F----7F7F7F7F-7....\n\
\.|F--7||||||||FJ....\n\
\.||.FJ||||||||L7....\n\
\FJL7L7LJLJ||LJ.L-7..\n\
\L--J.L7...LJS7F-7L7.\n\
\....F-J..F7FJ|L7L7L7\n\
\....L7.F7||L7|.L7L7|\n\
\.....|FJLJ|FJ|F7|.LJ\n\
\....FJL-7.||.||||...\n\
\....L---J.LJ.LJLJ..."

test3 = "\
\FF7FSF7F7F7F7F7F---7\n\
\L|LJ||||||||||||F--J\n\
\FL-7LJLJ||||||LJL-77\n\
\F--JF--7||LJLJ7F7FJ-\n\
\L---JF-JLJ.||-FJLJJ7\n\
\|F|F-JF---7F7-L7L|7|\n\
\|FFJF7L7F-JF7|JL---7\n\
\7-L-JL7||F7|L7F-7F7|\n\
\L.L7LFJ|||||FJL7||LJ\n\
\L7JLJL-JLJLJL--JLJ.L"

test4 = "\
\.....\n\
\.S-7.\n\
\.|.|.\n\
\.L-J.\n\
\....."

test5 = "\
\-L|F7\n\
\7S-7|\n\
\L|7||\n\
\-L-J|\n\
\L|-JF"

test = "\
\..F-7..\n\
\..|.|..\n\
\F-J.L-7\n\
\|.....S\n\
\L-7.F-J\n\
\..|.|..\n\
\..L-J.."

-- process input = path
process input = Set.size $ dfs Set.empty cleanGrid startingPoints
    where (start, grid) = replaceStart $ parseGrid input
          path = traversalPath grid start
          cleanGrid = cleanUp grid path
          startingPoints = collectInnerSegments cleanGrid (traversalDirection path) path

data Pipe = Vert | Horiz | LD | RD | LU | RU | None | Start deriving (Show, Eq, Ord)

parsePipe '|' = Vert
parsePipe '-' = Horiz
parsePipe '7' = LD
parsePipe 'J' = LU
parsePipe 'F' = RD
parsePipe 'L' = RU
parsePipe 'S' = Start
parsePipe _   = None

parseGrid input = listArray ((0,0),(height-1,width-1)) $ map parsePipe $ filter (/= '\n') input
    where height = length $ lines input
          width = length $ head $ lines input

connect (y, x) Vert  = [(y-1, x), (y+1, x)]
connect (y, x) Horiz = [(y, x-1), (y, x+1)]
connect (y, x) LD    = [(y, x-1), (y+1, x)]
connect (y, x) RD    = [(y, x+1), (y+1, x)]
connect (y, x) LU    = [(y, x-1), (y-1, x)]
connect (y, x) RU    = [(y, x+1), (y-1, x)]
connect _ _          = []

isPipe None  = False
isPipe Start = False
isPipe _     = True

neighbours (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

inBounds grid (y, x) = sy <= y && y <= ey && sx <= x && x <= ex
    where ((sy,sx),(ey, ex)) = bounds grid

connectsTo pipe (py, px) (targetY, targetX) = (targetY, targetX) `elem` connect (py, px) pipe

data Side = L | R | U | D deriving (Show, Eq, Ord, Enum, Bounded)

displacement U (y, x) = (y-1, x)
displacement D (y, x) = (y+1, x)
displacement L (y, x) = (y, x-1)
displacement R (y, x) = (y, x+1)

replaceStart grid =
      (pos,)
    $ (grid //)
    $ (:[])
    $ (pos,)
    $ pipeFor
    $ map fst
    $ filter (isConnected . snd)
    $ filter (inBounds grid . snd)
    $ map sidedNeighbours
    [minBound..maxBound]
    where Just (pos, Start) = find ((== Start) . snd) $ assocs grid
          sidedNeighbours dir = (dir, displacement dir pos)
          isConnected neighbour = connectsTo (grid ! neighbour) neighbour pos

pipeFor [U, D] = Vert
pipeFor [L, R] = Horiz
pipeFor [L, U] = LU
pipeFor [L, D] = LD
pipeFor [R, U] = RU
pipeFor [R, D] = RD
pipeFor [x, y] | x == y = error "Invalid pipe"
pipeFor [x, y] | x > y = pipeFor [y, x]

data State = State {
    grid      :: Array (Int, Int) Pipe,
    start     :: (Int, Int),
    incomming :: Side,
    current   :: (Int, Int),
    path      :: [Node]
} deriving (Show, Eq, Ord)

data Node = Node {
    pos       :: (Int, Int),
    pipe      :: Pipe,
    direction :: Side
} deriving (Show, Eq, Ord)

data Direction = Clockwise | CounterClockwise deriving (Show, Eq, Ord, Enum, Bounded)

nextSide U Vert  = U
nextSide D Vert  = D
nextSide R Horiz = R
nextSide L Horiz = L
nextSide U LD    = L
nextSide R LD    = D
nextSide R LU    = U
nextSide D LU    = L
nextSide U RD    = R
nextSide L RD    = D
nextSide D RU    = R
nextSide L RU    = U

pickSide Vert  = (U, U)
pickSide Horiz = (R, R)
pickSide LD    = (R, D)
pickSide RD    = (U, R)
pickSide LU    = (R, U)
pickSide RU    = (D, R)

traversalPath grid from = reverse $ traversalPath' State {
    grid = grid,
    start = from,
    incomming = wentTo,
    current = nextPos,
    path = [node]
    }
    where pipe = grid ! from
          (cameFrom, wentTo) = pickSide pipe
          nextPos = displacement wentTo from
          node = Node { pos = from, pipe = pipe, direction = cameFrom }

traversalPath' state@(State { current, start, path, grid, incomming })
    | current == start = path
    | otherwise        = traversalPath' state { current = nextPos, incomming = nextIncomming, path = node:path }
    where pipe = grid ! current
          nextIncomming = nextSide incomming pipe
          nextPos = displacement nextIncomming current
          node = Node { pos = current, pipe = pipe, direction = incomming }

turns = map pos . filter ((`elem` [LU, RU, LD, RD]) . pipe)

zipCyclic xs = zip xs $ tail $ cycle xs

signedArea turns = sum $ map (\((x1, y1), (x2, y2)) -> (x2 - x1) * (y2 + y1)) $ zipCyclic turns

traversalDirection path
    | area > 0 = Clockwise
    | area < 0 = CounterClockwise
    where area = signedArea $ turns path

collectInnerSegments grid dir = concatMap (filter (isNone grid) . filter (inBounds grid) . innerSegment dir)

innerSegment Clockwise        (Node { pipe = Vert , direction = U, pos }) = [displacement R pos]
innerSegment Clockwise        (Node { pipe = Vert , direction = D, pos }) = [displacement L pos]
innerSegment Clockwise        (Node { pipe = Horiz, direction = R, pos }) = [displacement D pos]
innerSegment Clockwise        (Node { pipe = Horiz, direction = L, pos }) = [displacement U pos]
innerSegment Clockwise        (Node { pipe = LD   , direction = U, pos }) = [displacement R pos, displacement U pos]
innerSegment Clockwise        (Node { pipe = LD   , direction = R, pos }) = []
innerSegment Clockwise        (Node { pipe = LU   , direction = R, pos }) = [displacement R pos, displacement D pos]
innerSegment Clockwise        (Node { pipe = LU   , direction = D, pos }) = []
innerSegment Clockwise        (Node { pipe = RD   , direction = L, pos }) = [displacement L pos, displacement U pos]
innerSegment Clockwise        (Node { pipe = RD   , direction = U, pos }) = []
innerSegment Clockwise        (Node { pipe = RU   , direction = D, pos }) = [displacement L pos, displacement D pos]
innerSegment Clockwise        (Node { pipe = RU   , direction = L, pos }) = []

innerSegment CounterClockwise (Node { pipe = Vert , direction = U, pos }) = [displacement L pos]
innerSegment CounterClockwise (Node { pipe = Vert , direction = D, pos }) = [displacement R pos]
innerSegment CounterClockwise (Node { pipe = Horiz, direction = R, pos }) = [displacement U pos]
innerSegment CounterClockwise (Node { pipe = Horiz, direction = L, pos }) = [displacement D pos]
innerSegment CounterClockwise (Node { pipe = LD   , direction = U, pos }) = []
innerSegment CounterClockwise (Node { pipe = LD   , direction = R, pos }) = [displacement R pos, displacement U pos]
innerSegment CounterClockwise (Node { pipe = LU   , direction = R, pos }) = []
innerSegment CounterClockwise (Node { pipe = LU   , direction = D, pos }) = [displacement R pos, displacement D pos]
innerSegment CounterClockwise (Node { pipe = RD   , direction = L, pos }) = []
innerSegment CounterClockwise (Node { pipe = RD   , direction = U, pos }) = [displacement L pos, displacement U pos]
innerSegment CounterClockwise (Node { pipe = RU   , direction = D, pos }) = []
innerSegment CounterClockwise (Node { pipe = RU   , direction = L, pos }) = [displacement L pos, displacement D pos]

innerSegment dir node = error $ "Invalid direction: " ++ show dir ++ " for node: " ++ show node

cleanUp grid path = empty // map (\(Node {pipe, pos}) -> (pos, pipe)) path
    where empty = listArray (bounds grid) $ repeat None

dfs visited _ [] = visited
dfs visited grid (pos:queue)
    | Set.member pos visited = dfs visited grid queue
    | otherwise = dfs newVisited grid newQueue
    where pipe = grid ! pos
          connected = filter (not . (`Set.member` visited)) $ filter (isNone grid) $ neighbours pos
          newVisited = Set.insert pos visited
          newQueue = queue ++ connected

isNone grid pos = grid ! pos == None
