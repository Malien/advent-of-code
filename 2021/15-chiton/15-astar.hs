{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Array.Unboxed
import Data.Word
import Data.Char
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQueue
import Debug.Trace
import qualified Data.Set as Set
import Data.Set (Set)

main = readFile "input.txt" >>= print . solution

solution str = Main.traverse grid end $ PQueue.singleton cost startingPath
    where grid = parseGrid str
          (_, end) = bounds grid
          -- end = (20, 20)
          startingPath = Path { cost = 0, pos = (0, 0), trace = Set.singleton (0,0) }
          cost = computeCost end startingPath

parseGrid :: String -> Array (Int, Int) Word8
parseGrid str = listArray ((0, 0), (h - 1, w - 1)) . map (fromIntegral . digitToInt) . concat . lines $ str
    where (h, w) = (length $ lines str, length $ head $ lines str)

neighbours bounds (y, x) = filter (inRange bounds) [(y+1, x), (y, x+1), (y-1, x), (y, x-1)]

data Path = Path 
    { cost :: Int
    , pos  :: (Int, Int)
    , trace :: Set (Int, Int)
    } deriving Show

nextPaths :: Array (Int, Int) Word8 -> Path -> [Path]
nextPaths grid (Path { cost, pos, trace }) = 
    map toPath $ filter (not . (`Set.member` trace)) $ neighbours (bounds grid) pos
    where toPath newPos = Path 
            { cost = cost + fromIntegral (grid!newPos)
            , pos = newPos
            , trace = Set.insert newPos trace
            }

traverse grid end queue = processNode grid end newQ path
    where ((_, path), newQ) = PQueue.deleteFindMin queue

processNode :: Array (Int, Int) Word8 -> (Int, Int) -> MinPQueue Float Path -> Path -> Path
processNode _ end _ p | end == pos p = p
processNode grid end queue p = 
    Main.traverse grid end updatedQ
    where updatedQ = foldl insertPath queue (nextPaths grid p)
          insertPath :: MinPQueue Float Path -> Path -> MinPQueue Float Path
          insertPath q p = PQueue.insert (computeCost end p) p q

bias :: Float
bias = 2.2

computeCost :: (Int, Int) -> Path -> Float
computeCost (ex, ey) (Path { cost, pos = (x, y), .. }) = fromIntegral cost + bias * fromIntegral ((ex - x) + (ey - y))
-- computeCost (ex, ey) (Path { cost }) = cost

square x = x * x
pow3 x = x * x * x

