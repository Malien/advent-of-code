import           Data.Array (Array, assocs, bounds, listArray, (!))
import           Data.List  (find)
import           Data.Set   (Set)
import qualified Data.Set   as Set

main = readFile "in" >>= print . process

test1 = "\
\.....\n\
\.S-7.\n\
\.|.|.\n\
\.L-J.\n\
\....."

test2 = "\
\-L|F7\n\
\7S-7|\n\
\L|7||\n\
\-L-J|\n\
\L|-JF"

test = "\
\..F7.\n\
\.FJ|.\n\
\SJ.L7\n\
\|F--J\n\
\LJ..."

test4 = "\
\7-F7-\n\
\.FJ|7\n\
\SJLL7\n\
\|F--J\n\
\LJ.LJ"

process input = maximum $ dfs $ State
        { grid = grid
        , visited = Set.singleton pos
        , queue = map (,1) $ startConnections grid pos
        , acc = [] }
    where grid = parseGrid input
          Just (pos, Start) = find ((== Start) . snd) $ assocs grid

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

data State = State {
    grid    :: Array (Int, Int) Pipe,
    visited :: Set (Int, Int),
    queue   :: [((Int, Int), Int)],
    acc     :: [Int]
} deriving (Show, Eq, Ord)

neighbours (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

inBounds (y, x) grid = sy <= y && y <= ey && sx <= x && x <= ex
    where ((sy,sx),(ey, ex)) = bounds grid

startConnections grid pos = [neighbour | neighbour <- neighbours pos, inBounds neighbour grid, connectsTo (grid ! neighbour) neighbour pos]

connectsTo pipe (py, px) (targetY, targetX) = (targetY, targetX) `elem` connect (py, px) pipe

dfs (State { queue = [], acc }) = acc
dfs state@(State { visited, queue = ((pos, distance):queue), acc, grid })
    | Set.member pos visited = dfs state { queue = queue }
    | otherwise = dfs state {
        visited = Set.insert pos visited,
        queue = queue ++ connected,
        acc = distance:acc
    }
    where pipe = grid ! pos
          connected = map (, distance + 1) $ filter (not . (`Set.member` visited)) $ connect pos pipe
