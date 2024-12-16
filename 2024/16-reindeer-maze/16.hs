{-# LANGUAGE QuasiQuotes #-}

import           Data.Array        (Ix (range), listArray, (!))
import           Data.List         (find)
import qualified Data.PQueue.Min   as PQ
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Text.RawString.QQ (r)

main = readFile "in" >>= print . process

test = tail [r|
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

test' = tail [r|
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

process input = deijkstra grid end Set.empty (PQ.singleton (Entry start E 0))
  where (grid, start, end) = parseGrid input

data Direction = N | E | S | W deriving (Show, Eq, Ord)

data Entry = Entry (Int, Int) Direction Int deriving (Show)

instance Eq Entry where
  (Entry _ _ c1) == (Entry _ _ c2) = c1 == c2

instance Ord Entry where
  compare (Entry _ _ c1) (Entry _ _ c2) = compare c1 c2

deijkstra grid end visited queue =
  let Just (Entry position direction cost, queue') = PQ.minView queue in
  if position == end then cost
  else if (position, direction) `Set.member` visited then deijkstra grid end visited queue'
  else let
    visited' = Set.insert (position, direction) visited
    ns =
      [ e
      | e@(Entry p d c) <- neighbors position direction cost
      , grid ! p /= Wall
      , (p, d) `Set.notMember` visited'
      ]
    queue'' = foldr PQ.insert queue' ns
  in deijkstra grid end visited' queue''

neighbors p@(y, x) N cost = [Entry (y-1, x) N $ cost+1, Entry p E $ cost+1000, Entry p W $ cost+1000]
neighbors p@(y, x) E cost = [Entry (y, x+1) E $ cost+1, Entry p N $ cost+1000, Entry p S $ cost+1000]
neighbors p@(y, x) S cost = [Entry (y+1, x) S $ cost+1, Entry p E $ cost+1000, Entry p W $ cost+1000]
neighbors p@(y, x) W cost = [Entry (y, x-1) W $ cost+1, Entry p N $ cost+1000, Entry p S $ cost+1000]

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
