{-# LANGUAGE QuasiQuotes #-}

import           Data.Array
import           Data.List         (partition, sortOn)
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

test3 = tail [r|
AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA
|]

test4 = tail [r|
OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
|]

test5 = tail [r|
EEEEE
EXXXX
EEEEE
EXXXX
EEEEE
|]

test6 = tail [r|
0000000
00WW000
000WW00
000WW00
00WWW00
0000000
|]

process input = sum $ map (uncurry (*)) $ discoverRegion grid $ Set.fromList $ indices grid
  where grid = parse input

discoverRegion grid unprocessed = case Set.minView unprocessed of
  Nothing -> []
  Just (x, unprocessed') ->
    (area, sides) : discoverRegion grid (Set.difference unprocessed' traversed)
    where (traversed, perimeter) = bfs grid (grid ! x) Set.empty [] [x]
          area = Set.size traversed
          sides = countAllSides $ sortOn snd perimeter

parse input = listArray ((1,1), (height, width)) $ filter (/= '\n') input
  where height = length $ lines input
        width = length $ head $ lines input

bfs grid area visited exteriors [] = (visited, exteriors)
bfs grid area visited exteriors (pos:queue)
  | pos `Set.member` visited = bfs grid area visited exteriors queue
  | otherwise = bfs grid area visited' (exteriors ++ external) queue'
  where isInternal neighbor = inRange (bounds grid) neighbor && (grid ! neighbor) == area
        (internal, external)
          = partition (isInternal . snd)
          . map (\direction -> (direction, offset pos direction))
          $ [minBound..maxBound]
        queue' = queue ++ map snd internal
        visited' = Set.insert pos visited

data Direction = North | East | South | West deriving (Show, Eq, Enum, Bounded)

offset (y, x) North = (y+1, x)
offset (y, x) East  = (y, x+1)
offset (y, x) South = (y-1, x)
offset (y, x) West  = (y, x-1)

isVertical East = True
isVertical West = True
isVertical _    = False

isHorizontal = not . isVertical

countSides []              = []
countSides (exterior:rest) = countSides' [] exterior rest

type Exterior = (Direction, (Int, Int))
countSides' :: [Exterior] -> Exterior -> [Exterior] -> [Exterior]
countSides' acc prev@(prevSide, (prevY, prevX)) (next@(nextSide, (nextY, nextX)):rest)
  | isVertical prevSide && prevSide == nextSide && prevY + 1 == nextY && prevX == nextX
  = countSides' acc next rest
  | isHorizontal prevSide && prevSide == nextSide && prevX + 1 == nextX && prevY == nextY
  = countSides' acc next rest
  | otherwise = countSides' (next:acc) prev rest
countSides' acc _ [] = reverse acc

countAllSides = length . takeWhile (not . null) . iterate countSides
