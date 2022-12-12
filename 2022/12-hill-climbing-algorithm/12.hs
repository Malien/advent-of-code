{-# LANGUAGE NamedFieldPuns #-}
import           Data.Array
import           Data.Char       (ord)
import           Data.List       (intersperse)
import           Data.List.Split (chunksOf)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import qualified Data.PQueue.Min as PQueue

main = process <$> readFile "in" >>= print

process inp = complexity <$> Map.lookup endPos nodes
  where grid      = parseMap inp
        startPos  = findNode 'S' grid
        endPos    = findNode 'E' grid
        startNode = Node { pos = startPos, complexity = 0 }
        nodes     = traverseGrid grid Map.empty PQueue.empty startNode

traverseGrid grid visited queue fromNode@Node { pos = from }
  | from `Map.member` visited && PQueue.null queue = visited
  | from `Map.member` visited = traverseGrid grid visited visitedNextQueue visitedNextNode
  | PQueue.null newQueue      = Map.insert from fromNode visited
  | otherwise                 = traverseGrid grid (Map.insert from fromNode visited) queueWithoutNode nextNode
  where newQueue = newQueue_
        newQueue_ =
          foldr (PQueue.insert) queue
          $ map (toNode fromNode)
          $ filter (not . (`Map.member` visited))
          $ filter (traversable grid from)
          $ filter (inRange (bounds grid))
          $ neighbors from
        (nextNode, queueWithoutNode) = PQueue.deleteFindMin newQueue
        (visitedNextNode, visitedNextQueue) = PQueue.deleteFindMin queue

toNode Node { complexity } pos = Node { pos, complexity = complexity + 1 }

neighbors (y, x) = [(y+1,x), (y,x+1), (y, x-1), (y-1,x)]

traversable grid from to = toHeight - fromHeight <= 1
  where fromHeight = cellHeight $ grid ! from
        toHeight = cellHeight $ grid ! to

parseMap def = listArray ((1,1), mapBounds def) $ filter (/= '\n') def

mapBounds def = (length l, length $ head l)
  where l = lines def

cellHeight 'S' = cellHeight 'a'
cellHeight 'E' = cellHeight 'z'
cellHeight x   = ord x - ord 'a'

findNode node = fst . head . filter ((== node) . snd) . assocs

data Node = Node {
  pos        :: (Int, Int),
  complexity :: Int
} deriving Show

instance Eq Node where
  a == b = complexity a == complexity b

instance Ord Node where
  compare a b = compare (complexity a) (complexity b)

test =
  "Sabqponm\n\
  \abcryxxl\n\
  \accszExk\n\
  \acctuvwj\n\
  \abdefghi"
