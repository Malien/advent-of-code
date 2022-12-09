{-# LANGUAGE NamedFieldPuns #-}
import           Data.Set (Set)
import qualified Data.Set as Set
import           Prelude  hiding (Left, Right)

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

process = 
    Set.size 
  . visited 
  . foldl advance startingRope
  . concatMap repeatMoves 
  . map moves 
  . lines

moves line = (parseDirection direction, read count :: Int)
  where [direction, count] = words line

repeatMoves (move, count) = take count $ repeat move

type Pos = (Int, Int)
data Rope = Rope { rHead :: Pos, rTail :: Pos, visited :: Set Pos } deriving Show
data Direction = Up | Down | Left | Right

startingRope = Rope { rHead = (0,0), rTail = (0,0), visited = Set.singleton (0,0) }

parseDirection "R" = Right
parseDirection "L" = Left
parseDirection "U" = Up
parseDirection "D" = Down

adjacent (ax, ay) (bx, by) = dx <= 1 && dy <= 1
  where dx = abs (ax - bx)
        dy = abs (ay - by)

movePoint (x, y) Right = (x + 1, y)
movePoint (x, y) Left  = (x - 1, y)
movePoint (x, y) Up    = (x, y + 1)
movePoint (x, y) Down  = (x, y - 1)

advance (Rope { rHead, rTail, visited }) direction
  | adjacent moved rTail = Rope { rHead = moved, rTail, visited }
  | otherwise = Rope { rHead = moved, rTail = rHead, visited = Set.insert rHead visited }
  where moved = movePoint rHead direction
