{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes         #-}

import           Data.Array
import           Data.List         (find)
import           Data.List.Split   (chunksOf)
import           Data.Map          (Map)
import qualified Data.Map          as Map
import qualified Data.PQueue.Min   as PQ
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Text.RawString.QQ (r)

main = readFile "in" >>= process

test' = tail [r|
###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
|]

test = tail [r|
#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
|]

-- process input = putStr $ display grid $ (end:) $ concatMap path $ deijkstra [] grid end Map.empty queue
process input = print $ Set.size $ Set.fromList $ (end:) $ concatMap path $ deijkstra [] grid end Map.empty queue
  where (grid, start, end) = parseGrid input
        queue = PQ.singleton Entry {
            position = start,
            direction = E,
            cost = 0,
            path = [start]
          }

display grid positions = unlines $ chunksOf (snd $ snd $ bounds grid) $ elems pathed
  where blank = sd <$> grid
        pathed = blank // [ (pos, 'O') | pos <- positions ]

sd Wall  = '#'
sd Empty = ' '

data Direction = N | E | S | W deriving (Show, Eq, Ord)

data Entry = Entry {
  position  :: (Int, Int),
  direction :: Direction,
  cost      :: Int,
  path      :: [(Int, Int)]
} deriving (Show)

instance Eq Entry where
  a == b = a.cost == b.cost

instance Ord Entry where
  compare a b = compare a.cost b.cost

shouldProceed entry visited = case Map.lookup (entry.position, entry.direction) visited of
  Nothing   -> True
  Just cost -> entry.cost <= cost

deijkstra ::
  [Entry] ->
  Array (Int, Int) Tile ->
  (Int, Int) ->
  Map ((Int, Int), Direction) Int ->
  PQ.MinQueue Entry ->
  [Entry]
deijkstra [] grid end visited queue =
  let Just (entry, queue') = PQ.minView queue in
  if entry.position == end then deijkstra [entry] grid end visited queue'
  else if not $ shouldProceed entry visited then deijkstra [] grid end visited queue'
  else let
    visited' = Map.insert (entry.position, entry.direction) entry.cost visited
    ns =
      [ neighbor
      | neighbor <- neighbors entry
      , grid ! neighbor.position /= Wall
      , shouldProceed neighbor visited'
      ]
    queue'' = foldr PQ.insert queue' ns
  in deijkstra [] grid end visited' queue''

deijkstra acc@(Entry { cost = targetCost }:_) grid end visited queue =
  let Just (entry, queue') = PQ.minView queue in
  if entry.cost > targetCost then acc
  else if entry.position == end then deijkstra (entry:acc) grid end visited queue'
  else if not $ shouldProceed entry visited then deijkstra acc grid end visited queue'
  else let
    visited' = Map.insert (entry.position, entry.direction) entry.cost visited
    ns =
      [ neighbor
      | neighbor <- neighbors entry
      , grid ! neighbor.position /= Wall
      , shouldProceed neighbor visited'
      ]
    queue'' = foldr PQ.insert queue' ns
  in deijkstra acc grid end visited' queue''

neighbors entry@(Entry { position = (y, x), direction = N, cost, path }) =
  [ entry { position = (y-1, x), cost = cost+1, path = (y, x) : path }
  , entry { direction = E, cost = cost+1000}
  , entry { direction = W, cost = cost+1000}
  ]
neighbors entry@(Entry { position = (y, x), direction = E, cost, path }) =
  [ entry { position = (y, x+1), cost = cost+1, path = (y, x) : path }
  , entry { direction = N, cost = cost+1000}
  , entry { direction = S, cost = cost+1000}
  ]
neighbors entry@(Entry { position = (y, x), direction = S, cost, path }) =
  [ entry { position = (y+1, x), cost = cost+1, path = (y, x) : path }
  , entry { direction = E, cost = cost+1000}
  , entry { direction = W, cost = cost+1000}
  ]
neighbors entry@(Entry { position = (y, x), direction = W, cost, path }) =
  [ entry { position = (y, x-1), cost = cost+1, path = (y, x) : path }
  , entry { direction = N, cost = cost+1000}
  , entry { direction = S, cost = cost+1000}
  ]

parseGrid input = (grid, s, e)
  where h = length $ lines input
        w = length $ head $ lines input
        grid = listArray ((1,1), (h,w)) [parseTile c | c <- input, c /= '\n']
        indexed = zip (range ((1, 1), (h, w))) (filter (/= '\n') input)
        Just (s, _) = find ((== 'S') . snd) indexed
        Just (e, _) = find ((== 'E') . snd) indexed

data Tile = Wall | Empty deriving (Eq)

parseTile '#' = Wall
parseTile '.' = Empty
parseTile 'S' = Empty
parseTile 'E' = Empty

instance Show Tile where
  show Wall  = "#"
  show Empty = "."
