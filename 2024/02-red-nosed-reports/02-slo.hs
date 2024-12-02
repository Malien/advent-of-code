import           Data.List
import           Data.List.Split
import           Data.Ord
import           Data.Char
import           Data.Maybe
import           Debug.Trace
import           Data.Array
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

main = readFile "in" >>= putStr . process

test = "\
\9 2 3 4 5\n\
\1 9 3 4 5\n\
\9 4 3 2 1\n\
\4 9 3 2 1\n\
\1 2 3 4 9\n\
\1 2 3 9 4\n\
\5 4 3 2 9\n\
\5 4 3 9 2\n\
\41 38 40 42 44 47\n\
\12 15 9 7 6 5\n\
\"

test2 = "\
\7 6 4 2 1\n\
\1 2 7 8 9\n\
\9 7 6 2 1\n\
\1 3 2 4 5\n\
\8 6 4 4 1\n\
\1 3 6 7 9\n\
\"

process input = unlines . map (show . fst) . filter (not . snd) $ zip rows . map lenientSlope $ rows
  where rows = parse input

parse :: String -> [[Int]]
parse = map (map read . words) . lines

data Slope = Increasing | Decreasing | Unknown deriving (Show)

perms xs = perms' xs [] xs []

perms' [] head tail acc = head:acc
perms' (x:xs) head (_:tail) acc = perms' xs (head ++ [x]) tail ((head ++ tail):acc)

lenientSlope row = any (safeSlope Unknown) $ perms row

safeSlope Unknown (x:y:xs)
  | x == y = False
  | x < y && y - x <= 3 = safeSlope Increasing (y:xs)
  | x > y && x - y <= 3 = safeSlope Decreasing (y:xs)
  | otherwise = False

safeSlope Increasing (x:y:xs)
  | x == y = False
  | y < x = False
  | y - x <= 3 = safeSlope Increasing (y:xs)
  | otherwise = False

safeSlope Decreasing (x:y:xs)
  | x == y = False
  | y > x = False
  | x - y <= 3 = safeSlope Decreasing (y:xs)
  | otherwise = False

safeSlope _ _ = True

