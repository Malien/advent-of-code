{-# LANGUAGE QuasiQuotes #-}

import           Data.Array
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Text.RawString.QQ (r)

main = readFile "in" >>= print . process

test = tail [r|
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
|]

test2 = tail [r|
AAAA
BBCD
BBCC
EEEC
|]

process input = sum $ map (uncurry (*)) $ discoverRegion grid $ Set.fromList $ indices grid
  where grid = parse input

discoverRegion :: Array (Int, Int) Char -> Set (Int, Int) -> [(Int, Int)]
discoverRegion grid unprocessed = case Set.minView unprocessed of
  Nothing -> []
  Just (x, unprocessed') ->
    (Set.size traversed, perimeter) : discoverRegion grid (Set.difference unprocessed' traversed)
    where (traversed, perimeter) = bfs grid (grid ! x) Set.empty 0 [x]

parse input = listArray ((1,1), (height, width)) $ filter (/= '\n') input
  where height = length $ lines input
        width = length $ head $ lines input

bfs grid area visited exteriors [] = (visited, exteriors)
bfs grid area visited exteriors (pos:rest)
  | pos `Set.member` visited = bfs grid area visited exteriors rest
  | otherwise = bfs grid area (Set.insert pos visited) (exteriors + exteriors') (rest ++ neighbors')
  where neighbors'
          = filter ((== area) . (grid !))
          $ filter (inRange (bounds grid))
          $ neighbors pos
        exteriors' = 4 - length neighbors'

neighbors (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
