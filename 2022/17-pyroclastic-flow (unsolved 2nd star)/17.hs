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

process = Set.findMax . (!! 2022) . rounds . map parseMovement . filter (/= '\n')
-- process = map parseMovement

displayGrid grid = unlines $ chunksOf 7 $ Array.elems $ base Array.// [((maxY - y, x), '#') | (y, x) <- Set.elems grid]
  where maxY | Set.null grid = 0
             | otherwise     = fst $ Set.findMax grid
        base = Array.listArray ((0,0), (maxY, 6)) $ repeat '.'

shapesTemplate = map (shapeArray . lines) [
  "####",

  ".#.\n\
  \###\n\
  \.#.",

  "###\n\
  \..#\n\
  \..#",

  "#\n\
  \#\n\
  \#\n\
  \#",

  "##\n\
  \##"
  ]

data State = State { grid :: Set (Int, Int), shapes :: [Array (Int, Int) Tile], movements :: [Movement] }

rounds movements = map grid $ iterate roundOfTetris State 
  { grid = Set.empty
  , shapes = cycle shapesTemplate
  , movements = cycle movements 
  }

roundOfTetris State { grid, shapes = (shape:nextShapes), movements } = State 
  { grid = nextGrid
  , shapes = nextShapes
  , movements = nextMovements
  }
  where (nextGrid, nextMovements) = dropShape grid shape (offsetY + 3, 2) movements
        offsetY | Set.null grid = 0
                | otherwise     = 1 + fst (Set.findMax grid)
        

data Tile = Filled | Hole deriving Show
isFilled Filled = True
isFilled Hole = False

shapeArray shape = Array.listArray bounds $ map parseTile $ concat shape
  where bounds = ((0,0), (length shape - 1, length (head shape) - 1))

shapeWidth = snd . snd . Array.bounds

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


applyShape grid offset = foldr Set.insert grid . shapePoints offset

isCompatible grid offset = not . any (`Set.member` grid) . shapePoints offset

shapePoints offset =
    map ((^+^ offset) . fst)
  . filter (isFilled . snd) 
  . Array.assocs 

(ax, ay) ^+^ (bx, by) = (ax + bx, ay + by)

test = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
