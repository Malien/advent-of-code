{-# LANGUAGE TupleSections #-}

import Data.List.Split
import qualified Data.Set as Set
import Data.Array.Unboxed
import Data.List (intersperse)

main = readFile "input.txt" >>= solution

solution str = 
    putStr .
    produceDisplay .
    toArray .
    foldl (\acc fold -> map (origami fold) acc) (parsePoints points) .
    parseFolds $
    folds
    where [points, folds] = splitOn "\n\n" str

produceDisplay :: Array (Int, Int) Char -> String
produceDisplay arr = (++ "\n") . foldl (++) "" . intersperse "\n" . chunksOf (end - start + 1) . elems $ arr
    where ((_, start), (_, end)) = bounds arr

swap (a, b) = (b, a)
   
toArray :: [(Int, Int)] -> Array (Int, Int) Char
toArray points = accumArray (\_ x -> x) ' ' ((0, 0), (maxY, maxX)) (map ((,'#') . swap) points)
    where maxX = maximum $ map fst points
          maxY = maximum $ map snd points

parsePoints = map parsePoint . lines
parsePoint :: String -> (Int, Int)
parsePoint line = (x, y)
    where [x, y] = map read $ splitOn "," line

data FoldCommand = Vertical Int | Horizontal Int deriving Show

foldCommand "y" = Horizontal
foldCommand "x" = Vertical

parseFolds = map parseFold . lines
parseFold str = foldCommand axis (read coord)
    where [axis, coord] = splitOn "=" $ drop 11 str

origami (Vertical pos) (x, y) | x > pos   = (2 * pos - x, y)
origami (Horizontal pos) (x, y) | y > pos = (x, 2 * pos - y)
origami _ coord = coord

