import Data.List.Split
import Debug.Trace
import Data.Maybe

main = readFile "input.txt" >>= print . solution

solution str = last . catMaybes . take 1000 . map (simulation bounds 0 0) $ [0..]
    where (_, bounds) = parseProblem str

simulation (ey, sy) pos max trajectory | pos > sy  = simulation 
                                                        (sy, ey) 
                                                        (pos + trajectory)
                                                        (Prelude.max pos max)
                                                        (trajectory - 1)
                                       | pos < ey  = Nothing
                                       | otherwise = Just max

parseProblem :: String -> ((Int, Int), (Int, Int))
parseProblem str = ((sx, ex), (sy, ey))
    where [[sx, ex], [sy, ey]] = map (map read . splitOn ".." . drop 2) . splitOn ", " . drop 13 $ str

