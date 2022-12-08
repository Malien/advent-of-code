{-# LANGUAGE TupleSections #-}
import           Data.Array
import           Data.Char

main = process <$> readFile "in" >>= print

test =
  "30373\n\
  \25512\n\
  \65332\n\
  \33549\n\
  \35390"

process inp = length . filter id . map (isVisible trees) $ indices trees
  where trees = parseMap inp

parseMap inp = listArray ((1,1), (mapBounds inp)) $ map digitToInt $ filter (/= '\n') inp

mapBounds inp = (length map, length $ head map)
  where map = lines inp

isVisible trees coord = any (stripeVisible trees $ trees ! coord) $ map (stripe coord $ bounds trees) directions

data Direction = West | North | East | South deriving (Enum, Bounded)
directions = [West .. South]

stripe (y, x) ((_, lowX), _)  West  = map (y,) [x, x-1 .. lowX]
stripe (y, x) ((lowY, _), _)  North = map (,x) [y, y-1 .. lowY]
stripe (y, x) (_, (_, highX)) East  = map (y,) [x .. highX]
stripe (y, x) (_, (highY, _)) South = map (,x) [y .. highY]

stripeVisible trees height = all (height >) . map (trees !) . tail
