{-# LANGUAGE QuasiQuotes #-}

import           Data.Array
import           Data.Char         (digitToInt)
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Text.RawString.QQ (r)

main = readFile "in" >>= print . process

test = tail [r|
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
|]

process input = sum $ map (score grid) zeros
  where grid = parse input
        zeros = [pos | (pos, v) <- assocs grid, v == 0]

parse input = listArray ((1,1), (height, width)) $ map digitToInt $ filter (/= '\n') input
  where height = length $ lines input
        width = length $ head $ lines input

dfs :: Array (Int, Int) Int -> Set (Int, Int) -> (Int, Int) -> Set (Int, Int)
dfs grid visited pos = foldr traverse (Set.insert pos visited) validNeighbors
  where validNeighbors = filter isValid $ neighbors pos
        traverse :: (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
        traverse neighbor visited = dfs grid (Set.insert neighbor visited) neighbor
        isValid neigbor =
          inRange (bounds grid) neigbor &&
          grid ! neigbor == grid ! pos + 1 &&
          not (Set.member neigbor visited)


neighbors (y, x) = [(y-1, x), (y+1, x), (y, x-1), (y, x+1)]

score grid = length . filter (== 9) . map (grid !) . Set.toList . dfs grid Set.empty
