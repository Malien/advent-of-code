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

parseCoord :: String -> (Int, Int)
parseCoord str = (x, y)
    where [x, y] = map read $ splitOn "," str

coordsBetween a@(x1, y1) b@(x2, y2) | x1 == x2 && y1 == y2 = [(x1, y1)]
                                    | x1 == x2 && y1 > y2 = coordsBetween b a
                                    | x1 == x2 = (x1, y1) : coordsBetween (x1, y1 + 1) b
                                    | y1 == y2 && x1 > x2 = coordsBetween b a
                                    | y1 == y2 = (x1, y1) : coordsBetween (x1 + 1, y1) b
                                    | otherwise = []


