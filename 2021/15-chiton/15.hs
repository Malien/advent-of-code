{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Array.Unboxed
import Data.Word
import Data.Char
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as PQueue
import qualified Data.Set as Set
import Data.Set (Set)

main = readFile "input.txt" >>= print . solution

solution str = Main.traverse grid end (PQueue.singleton startingPath) Set.empty
    where grid = parseGrid str
          (_, end) = bounds grid
          startingPath = Path { cost = 0, pos = (0, 0)  }

parseGrid :: String -> Array (Int, Int) Word8
parseGrid str = 
    array ((0, 0), (h * 5 - 1, w * 5 - 1)) . 
    repetitions w h .
    concatMap (\(i, row) -> map (\(j, cell) -> ((i, j), cell)) row) . 
    zip [0..] .
    map (zip [0..] . map (fromIntegral . digitToInt)) . 
    lines $ str
    where (h, w) = (length . lines  $ str, length . head . lines $ str)

repetitions :: Int -> Int -> [((Int, Int), Word8)] -> [((Int, Int), Word8)]
repetitions w h = 
    concatMap (\(i, (j, cells)) -> map (applyShift w h i j) cells) .
    concatMap (zip [0..4] . repeat) .
    zip [0..4] . repeat

replicateWrapped :: Int -> Word8 -> Word8
replicateWrapped x cell = fromIntegral $ 1 + (((fromIntegral cell) - 1 + x) `mod` 9)

applyShift w h i j ((y, x), cell) = ((y + h * i, x + w * j), replicateWrapped (i + j) cell)

neighbours bounds (y, x) = filter (inRange bounds) [(y+1, x), (y, x+1), (y-1, x), (y, x-1)]

data Path = Path 
    { cost :: Int
    , pos  :: (Int, Int)
    } deriving Show

instance Eq Path where
    a == b = cost a == cost b

instance Ord Path where
    compare a b = compare (cost a) (cost b)

nextPaths :: Array (Int, Int) Word8 -> Set (Int, Int) -> Path -> [Path]
nextPaths grid visited (Path { cost, pos }) = 
    map toPath $ filter (not . (`Set.member` visited)) $ neighbours (bounds grid) pos
    where toPath newPos = Path 
            { cost = cost + fromIntegral (grid!newPos)
            , pos = newPos
            }

minUnvisitedNode queue visited | pos path `Set.member` visited = minUnvisitedNode newQ visited
                               | otherwise = (path, newQ)
    where (path, newQ) = PQueue.deleteFindMin queue

traverse grid end queue visited = processNode grid end newQ newVisited path
    where (path, newQ) = minUnvisitedNode queue visited
          newVisited = Set.insert (pos path) visited

processNode :: Array (Int, Int) Word8 -> (Int, Int) -> MinQueue Path -> Set (Int, Int) -> Path -> Int
processNode _ end _ _ (Path { pos, cost }) | end == pos = cost
processNode grid end queue visited p = 
    Main.traverse grid end updatedQ visited
    where updatedQ = foldl insertPath queue (nextPaths grid visited p)
          insertPath :: MinQueue Path -> Path -> MinQueue Path
          insertPath q p = PQueue.insert p q

