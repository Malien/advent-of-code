import qualified Data.Array as Array
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe
import           Data.Ord
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Debug.Trace
import           Prelude         hiding (Left, Right)
import           Text.Regex.PCRE
import Data.Array (Array)

-- main = readFile "in" >>= putStr . process
main = readFile "in" >>= print . process
-- main = putStr $ process test

process = (!! 100_000_000) . rounds . map parseMovement . filter (/= '\n')
-- process = map parseMovement

-- shapesTemplate = map (shapeArray . lines) [
--   "####",

--   ".#.\n\
--   \###\n\
--   \.#.",

--   "###\n\
--   \..#\n\
--   \..#",

--   "#\n\
--   \#\n\
--   \#\n\
--   \#",

--   "##\n\
--   \##"
--   ]

shapesTemplate = map (\(res, req) -> Shape (boundlessArray res) (boundlessArray req)) [
  ([1,1,1,1], [0,0,0,0]),
  ([2,3,2], [1,0,1]),
  ([1,1,3], [0,0,0]),
  ([4], [0]),
  ([2,2], [0,0])
  ]

boundlessArray xs = Array.listArray (0, length xs - 1) xs


data Shape = Shape {
  resulting:: Array Int Int,
  required:: Array Int Int 
}

data State = State { grid :: Array Int Int, shapes :: [Shape], movements :: [Movement] }

rounds movements = map grid $ iterate roundOfTetris State 
  { grid = Array.listArray (0,6) $ repeat 0
  , shapes = cycle shapesTemplate
  , movements = cycle movements 
  }

roundOfTetris State { grid, shapes = (shape:nextShapes), movements } = State 
  { grid = nextGrid
  , shapes = nextShapes
  , movements = nextMovements
  }
  where (nextGrid, nextMovements) = dropShape grid shape (offsetY + 3, 2) movements
        offsetY = maximum grid
        

data Tile = Filled | Hole deriving Show
isFilled Filled = True
isFilled Hole = False

shapeArray shape = Array.listArray bounds $ map parseTile $ concat shape
  where bounds = ((0,0), (length shape - 1, length (head shape) - 1))

shapeWidth = snd . Array.bounds . resulting

parseTile '.' = Hole
parseTile '#' = Filled

data Movement = Left | Right deriving Show

parseMovement '<' = Left
parseMovement '>' = Right

-- offset is counted from the bottom left corner
dropShape grid shape (offsetY, offsetX) (Left:nextMoves) = moveBottom grid shape (offsetY, leftX) nextMoves
  where leftX | offsetX == 0 = 0
              | isCompatible grid (offsetY, offsetX - 1) shape = offsetX - 1
              | otherwise = offsetX
dropShape grid shape (offsetY, offsetX) (Right:nextMoves) = moveBottom grid shape (offsetY, rightX) nextMoves
  where rightX | offsetX + shapeWidth shape == 6 = offsetX
               | isCompatible grid (offsetY, offsetX + 1) shape = offsetX + 1
               | otherwise = offsetX

moveBottom grid shape offset@(0, _) moves = traceGrid (applyShape grid offset shape, moves)
  -- where traceGrid = traceShow (head moves) $ trace (displayGrid $ applyShape grid offset shape)
  where traceGrid = id
moveBottom grid shape (offsetY, offsetX) moves
  | isCompatible grid (offsetY - 1, offsetX) shape = traceGrid $ dropShape grid shape (offsetY - 1, offsetX) moves
  | otherwise = traceGrid (applyShape grid (offsetY, offsetX) shape, moves)
  -- where traceGrid = traceShow (head moves) $ trace (displayGrid $ applyShape grid (offsetY, offsetX) shape)
  where traceGrid = id


applyShape grid (offsetY, offsetX) (Shape { resulting }) = grid Array.// [(offsetX + dx, offsetY + height) | (dx, height) <- Array.assocs resulting]

isCompatible grid (offsetY, offsetX) (Shape { required }) = all id $ zipWith (>) shapeSection gridSection 
  where gridSection = drop offsetX $ Array.elems grid
        shapeSection = map (offsetY +) $ Array.elems required

(ax, ay) ^+^ (bx, by) = (ax + bx, ay + by)

test = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
