{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE TupleSections #-}

import           Data.Array        (Ix (inRange))
import           Data.List.Split   (splitOn)
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Text.RawString.QQ (r)

main = readFile "in" >>= print . process . (1024, 70,)

test :: (Int, Int, String)
test = (12, 6,) $ tail [r|
5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
|]

process (len, bound, input) = length $ bfs obstacles bound Set.empty [([], (0, 0))]
  where obstacles = Set.fromList $ take len $ map parse $ lines input

parse line = (y, x)
  where [x, y] = map read $ splitOn "," line

bfs obstacles bound visited ((path, pos@(y, x)):rest)
  | pos == (bound, bound) = path
  | pos `Set.member` visited = bfs obstacles bound visited rest
  | otherwise = bfs obstacles bound visited' rest'
  where valid
          = filter (inRange ((0,0), (bound, bound)))
          . filter (`Set.notMember` visited)
          . filter (`Set.notMember` obstacles)
          $ [(y+1, x), (y-1, x), (y, x+1), (y, x-1)]
        visited' = Set.insert pos visited
        rest' = rest ++ map (\p -> (p:path, p)) valid
