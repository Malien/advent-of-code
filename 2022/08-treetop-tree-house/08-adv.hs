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

process inp = maximum $ map (scenicScore trees) $ indices trees
  where trees = parseMap inp

parseMap inp = listArray ((1,1), (mapBounds inp)) $ map digitToInt $ filter (/= '\n') inp

mapBounds inp = (length map, length $ head map)
  where map = lines inp

scenicScore trees coord = product $ map (scenicScoreInDirection trees coord) directions

scenicScoreInDirection trees coord =
    stripeScenicScore
  . map (trees !)
  . stripe coord (bounds trees)

stripeScenicScore (height:rest) = stripeScenicScore' height rest
  where stripeScenicScore' _ [] = 0
        stripeScenicScore' height (nextTree:rest)
          | height > nextTree = 1 + stripeScenicScore' height rest
          | otherwise         = 1

data Direction = West | North | East | South deriving (Enum, Bounded)
directions = [West .. South]

stripe (y, x) ((_, lowX), _)  West  = map (y,) [x, x-1 .. lowX]
stripe (y, x) ((lowY, _), _)  North = map (,x) [y, y-1 .. lowY]
stripe (y, x) (_, (_, highX)) East  = map (y,) [x .. highX]
stripe (y, x) (_, (highY, _)) South = map (,x) [y .. highY]
