import Data.List.Split
import qualified Data.Map as Map

main = readFile "input.txt" >>= print . solution . lines

solution = Map.size
        . Map.filter (>1)
        . foldr (\key -> Map.insertWith (+) key 1) Map.empty 
        . concatMap (uncurry coordsBetween)
        . map parseLines

parseLines line = (start, end)
    where [start, end] = map parseCoord $ splitOn " -> " line

parseCoord str = (x, y)
    where [x, y] = map read $ splitOn "," str

coordsBetween a@(x1, y1) b@(x2, y2) | solvable  = coordsBetween' a (sx, sy) b []
                                    | otherwise = []
    where (dx, dy) = (x2 - x1, y2 - y1)
          (sx, sy) = (signum dx, signum dy)
          solvable = dx == 0 || dy == 0 || abs(dx) == abs(dy)

coordsBetween' point _ end accum | point == end = point:accum
coordsBetween' p@(x, y) s@(sx, sy) end accum = coordsBetween' (x + sx, y + sy) s end (p:accum)

