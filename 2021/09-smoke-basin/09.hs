import Data.Array.Unboxed
import Data.Char
import Data.Word
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Data.List

main = readFile "input.txt" >>= print . solutionB

solutionA = sum . map ((+1) . fromIntegral . snd) . findSinks . parseGrid
solutionB str = product . take 3 . sortBy (flip compare) . snd . foldl (collectBasins grid) (Set.empty, []) . map fst . findSinks $ grid
    where grid = parseGrid str

collectBasins grid (visited, acc) sink = (newVisited, size:acc)
    where (newVisited, size) = regionSize grid visited sink

type Grid = Array (Int, Int) Word8

parseGrid :: String -> Grid
parseGrid str = listArray ((0,0), (h-1, w-1)) . map (fromIntegral . digitToInt) . concat . lines $ str
    where ls = lines str
          (h, w) = (length ls, length $ head ls)

findSinks :: Grid -> [((Int, Int), Word8)]
findSinks grid = filter (isSink grid) $ assocs grid

neighbours grid (y, x) = 
    filter (inRange $ bounds grid) [(y, x-1), (y, x+1), (y-1, x), (y+1, x)]

connectedNeighbours grid pos = filter (\n -> grid!n < 9) $ neighbours grid pos 
 
isSink :: Grid -> ((Int, Int), Word8) -> Bool
isSink grid (pos, x) = all (\neighbour -> grid!neighbour > x) $ neighbours grid pos

idx grid = index (bounds grid)

regionSize grid visited pos = foldl (combineRegions grid) (Set.insert (idx grid pos) visited, 1) adjs
    where adjs = connectedNeighbours grid pos

combineRegions grid (visited, size) pos | (idx grid pos) `Set.member` visited = (visited, size)
combineRegions grid (visited, size) pos = (newVisited, newSize + size)
    where (newVisited, newSize) = regionSize grid visited pos

