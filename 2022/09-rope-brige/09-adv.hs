{-# LANGUAGE NamedFieldPuns #-}
import           Data.List (minimumBy)
import           Data.Ord  (comparing)
import           Data.Set  (Set)
import qualified Data.Set  as Set
import           Prelude   hiding (Left, Right)

main = process <$> readFile "in" >>= print

test =
  "R 4\n\
  \U 4\n\
  \L 3\n\
  \D 1\n\
  \R 4\n\
  \D 1\n\
  \L 5\n\
  \R 2"

test2 =
  "R 5\n\
  \U 8\n\
  \L 8\n\
  \D 3\n\
  \R 17\n\
  \D 10\n\
  \L 25\n\
  \U 20"

process =
    Set.size
  . visited
  . foldl advance startingPoint
  . concatMap repeatMoves
  . map moves
  . lines

moves line = (parseDirection direction, read count)
  where [direction, count] = words line

data Direction = Up | Down | Left | Right

parseDirection "R" = Right
parseDirection "L" = Left
parseDirection "U" = Up
parseDirection "D" = Down

repeatMoves (move, count) = take count $ repeat move

type Pos = (Int, Int)
data Rope = Rope { rHead :: Pos, knots :: [Pos], visited :: Set Pos }

startingPoint = Rope { rHead = (0, 0), knots = take 9 $ repeat (0,0), visited = Set.singleton (0,0) }

-- moveKnot (headX, headY) (knotX, knotY)
--   | headX - knotX >=  2 = (headX - 1, headY)
--   | headY - knotY >=  2 = (headX, headY - 1)
--   | headX - knotX >= -2 = (headX + 1, headY)
--   | headY - knotY >= -2 = (headX, headY + 1)
--   | otherwise           = (knotX, knotY)

advance (Rope { rHead, knots, visited }) direction = Rope
  { rHead   = newHead
  , knots   = newKnots
  , visited = Set.insert (last newKnots) visited
  }
  where newHead  = movePoint rHead direction
        newKnots = advanceKnots (newHead : knots)

movePoint (x, y) Right = (x + 1, y)
movePoint (x, y) Left  = (x - 1, y)
movePoint (x, y) Up    = (x, y + 1)
movePoint (x, y) Down  = (x, y - 1)

advanceKnots (prev:current:rest) = moved : (advanceKnots (moved : rest))
  where moved = moveKnot prev current
advanceKnots _ = []

moveKnot knotHead knot
  | adjacent knotHead knot = knot
  | otherwise              = minimumBy (comparing $ distance knotHead) $ map (applyDiff knot) neighbors

adjacent (ax, ay) (bx, by) = dx <= 1 && dy <= 1
  where dx = abs (ax - bx)
        dy = abs (ay - by)

neighbors = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1]]

distance (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)

applyDiff (x, y) (dx, dy) = (x + dx, y + dy)
