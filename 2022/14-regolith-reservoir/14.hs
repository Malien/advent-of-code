{-# LANGUAGE TupleSections #-}
import           Data.List       (unfoldr)
import           Data.List.Split (splitOn)
import           Data.Set        (Set)
import qualified Data.Set        as Set

main = readFile "in" >>= print . process

process inp = length $ simulations grid
  where grid = foldr (flip drawShape) Set.empty . map parseShape . lines $ inp

parseShape = map parseCoord . splitOn " -> "

simulations grid = iterateMaybe (simulateSand cutoff (500, 0)) grid
  where cutoff = abyssLine grid

iterateMaybe f = unfoldr (fmap (\s -> (s,s)) . f)

abyssLine = maximum . map snd . Set.elems

simulateSand cutoff (x,y) grid
  | y >= cutoff = Nothing
  | not (Set.member (x  , y+1) grid) = simulateSand cutoff (x  , y+1) grid
  | not (Set.member (x-1, y+1) grid) = simulateSand cutoff (x-1, y+1) grid
  | not (Set.member (x+1, y+1) grid) = simulateSand cutoff (x+1, y+1) grid
  | otherwise = Just $ Set.insert (x,y) grid

parseCoord line = (read x, read y) :: (Int, Int)
  where [x,y] = splitOn "," line

drawShape grid shape = foldl (uncurry . drawLine) grid $ zip shape (tail shape)

drawLine grid (startX, startY) (endX, endY)
  | startX == endX && startY <= endY = foldr Set.insert grid $ map (startX,) [startY .. endY]
  | startX == endX                   = foldr Set.insert grid $ map (startX,) [endY .. startY]
  | startY == endY && startX <= endX = foldr Set.insert grid $ map (,startY) [startX .. endX]
  | startY == endY                   = foldr Set.insert grid $ map (,startY) [endX .. startX]

test =
  "498,4 -> 498,6 -> 496,6\n\
  \503,4 -> 502,4 -> 502,9 -> 494,9"
