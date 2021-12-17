import Data.List.Split
import Debug.Trace
import Data.Maybe
import Data.Ix (inRange)

main = readFile "input.txt" >>= print . solution

solution str = length . filter id . map (simulateXY bounds (0,0)) $ cartesianProduct xs ys
    where bounds@((sx, ex), (ey, sy)) = parseProblem str
          ys = map snd . filter (fst . id) . map (\y -> (simulateY (ey, sy) 0 y, y)) $ [ey..1000]
          xs = map snd . filter (fst . id) . map (\x -> (simulateX (sx, ex) 0 x, x)) $ [0..ex]

simulateY (ey, sy) pos trajectory | pos > sy  = simulateY (ey, sy) (pos + trajectory) (trajectory - 1)
                                  | otherwise = pos >= ey

simulateX (sx, ex) pos 0 = pos >= sx && pos <= ex
simulateX (sx, ex) pos trajectory | pos < sx = simulateX (sx, ex) (pos + trajectory) (trajectory - 1)
                                  | otherwise = pos <= ex

simulateXY :: ((Int, Int), (Int, Int)) -> (Int, Int) -> (Int, Int) -> Bool
simulateXY bounds pos@(x, y) (dx, dy) | pos `within` bounds = True
                                      | pos `before` bounds = 
    simulateXY bounds (x + dx, y + dy) (max 0 (dx - 1), dy - 1)
                                      | otherwise = False

within (x, y) ((sx, ex), (ey, sy)) = x >= sx && x <= ex && y <= sy && y >= ey
before (x, y) ((sx, ex), (ey, sy)) = x <= ex && y >= ey

parseProblem :: String -> ((Int, Int), (Int, Int))
parseProblem str = ((sx, ex), (sy, ey))
    where [[sx, ex], [sy, ey]] = map (map read . splitOn ".." . drop 2) . splitOn ", " . drop 13 $ str

cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

