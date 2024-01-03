{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE QuasiQuotes           #-}
import           Data.Array
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Maybe
import           Data.Ord
import           Data.PQueue.Min   (MinQueue ((:<)))
import qualified Data.PQueue.Min   as PQ
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Debug.Trace
import           Text.RawString.QQ

main = readFile "in" >>= process

test = tail [r|
2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
|]

-- process inp = putStr $ drawPath (snd $ bounds grid) $ path $ findPath grid
process inp = print $ distance $ findPath grid
  where grid = parseGrid inp

parseGrid inp  = listArray ((1,1), (height, width)) $ map digitToInt $ filter (/= '\n') inp
  where height = length $ lines inp
        width  = length $ head $ lines inp

data State = State {
  grid      :: Array (Int, Int) Int,
  distances :: Map NodeKey Int,
  queue     :: MinQueue NodeState,
  target    :: (Int, Int)
}

instance Ord NodeState where
  compare = comparing distance

data NodeState = NodeState 
  { distance      :: Int
  , straightCount :: Int
  , from          :: Direction
  , pos           :: (Int, Int)
  -- , path          :: [NodeState]
  } deriving (Show, Eq)

data NodeKey = NodeKey 
  { pos           :: (Int, Int)
  , straightCount :: Int
  , from          :: Direction
  } deriving (Show, Eq, Ord)

nodeKey NodeState { pos, straightCount, from } = NodeKey { pos, straightCount, from }

data Direction = N | S | E | W deriving (Show, Eq, Ord)

dijkstra (State { target, queue = (node@NodeState { distance, pos }:<_) }) | target == pos = node
dijkstra state@(State { grid, distances, queue = (node:<queue) })
  = case distances Map.!? nodeKey node of
    Nothing                            -> dijkstra newState
    Just storedDistance
      | storedDistance < node.distance -> dijkstra state { queue = queue }
      | otherwise                      -> dijkstra newState
  where neighbourDistances = [NodeState
          { distance = node.distance + grid ! coord
          , pos = coord
          , from = dir
          , straightCount = if dir == node.from then node.straightCount + 1 else 1
          -- , path = node.path ++ [node]
          } | (dir, coord) <- accessibleNeighbours node (bounds grid)]
        newQueue = foldl (flip PQ.insert) queue neighbourDistances
        -- newVisited = Set.insert node.pos visited
        -- newDistances = distances // [(node.pos, Just node.distance)]
        newDistances = Map.insert (nodeKey node) node.distance distances
        newState = state { queue = newQueue, distances = newDistances }

-- accessibleNeighbours node bounds
--   | node.straightCount >= 3
--     = filter ((/= node.from) . fst)
--     $ neighbours bounds node.pos
--   | otherwise
--     = neighbours bounds node.pos

opposite N = S
opposite S = N
opposite E = W
opposite W = E

leftright N = [E, W]
leftright S = [E, W]
leftright E = [N, S]
leftright W = [N, S]

notBackwards N = [S, E, W]
notBackwards S = [N, E, W]
notBackwards E = [N, S, W]
notBackwards W = [N, S, E]

neighbour (y, x) N = (y-1, x)
neighbour (y, x) S = (y+1, x)
neighbour (y, x) E = (y, x+1)
neighbour (y, x) W = (y, x-1)

accessibleNeighbours :: NodeState -> ((Int, Int), (Int, Int)) -> [(Direction, (Int, Int))]
accessibleNeighbours node bounds =
  [ (dir, neighbour node.pos dir)
  | dir <- dirs
  , inRange bounds $ neighbour node.pos dir
  ]
  where dirs = if node.straightCount >= 3 then leftright node.from else [N, S, E, W]

neighbours bounds (y, x)
  = filter (inRange bounds . snd)
  [ (N, (y-1, x))
  , (S, (y+1, x))
  , (E, (y, x+1))
  , (W, (y, x-1))
  ]

findPath grid = dijkstra State {
  grid = grid,
  distances = Map.empty,
  queue = PQ.singleton NodeState 
    { distance = 0
    , straightCount = 0
    , from = E
    , pos = (1,1)
    -- , path = []
    },
  target = snd $ bounds grid
}

drawPath :: (Int, Int) -> [NodeState] -> String
drawPath size path = unlines $ chunksOf (snd size) $ elems grid'
  where grid = listArray ((1, 1), size) (repeat ' ')
        grid' = grid // [(node.pos, directionArrow node.from) | node <- path]

directionArrow N = '^'
directionArrow S = 'v'
directionArrow E = '>'
directionArrow W = '<'
