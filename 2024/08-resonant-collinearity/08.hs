{-# LANGUAGE QuasiQuotes #-}

import           Data.Array        (Ix (inRange, range))
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Text.RawString.QQ (r)

main = readFile "in" >>= print . process

test = tail [r|
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
|]

process input = length $ Set.fromList $ concatMap (antinodes bounds) $ Map.elems nodes
  where (nodes, bounds) = parse input

antinodes bounds antenas = [project a b | a <- antenas, b <- antenas, a /= b, inRange bounds (project a b)]
project (ay, ax) (by, bx) = (ay - by + ay, ax - bx + ax)

parse input = (nodes, ((1, 1), (height, width)))
  where height = length $ lines input
        width = length $ head $ lines input
        enumerated = zip (range ((1,1), (height, width))) (filter (/= '\n') input)
        nodes = foldr
          (uncurry (Map.insertWith (++)))
          Map.empty
          [(freq, [pos]) | (pos, freq) <- enumerated, freq /= '.']
