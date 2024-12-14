{-# LANGUAGE QuasiQuotes #-}

import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Maybe        (mapMaybe)
import           Text.RawString.QQ (r)
import           Text.Regex.PCRE   (AllTextMatches (getAllTextMatches), (=~))

main = readFile "in" >>= print . process' 103 101

test = tail [r|
=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
|]

process = process' 7 11

process' height width = product . Map.elems . count . mapMaybe (quadrant height width . simulate height width 100 . parse) . lines

data Robot = Robot { pos :: (Int, Int), vel :: (Int, Int) } deriving (Show)

parse line = Robot (y, x) (vy, vx)
  where [x, y, vx, vy] = map read $ getAllTextMatches $ line =~ "-?\\d+"

simulate height width times (Robot pos vel) = (wrap y height, wrap x width)
  where (y, x) = (vel ^* times) ^+^ pos

wrap x limit = ((x `mod` limit) + limit) `mod` limit

(a, b) ^+^ (c, d) = (a + c, b + d)
(a, b) ^* c = (a * c, b * c)

quadrant height width (y, x)
  | y < height `div` 2 && x < width `div` 2 = Just 1
  | y < height `div` 2 && x > width `div` 2 = Just 2
  | y > height `div` 2 && x < width `div` 2 = Just 3
  | y > height `div` 2 && x > width `div` 2 = Just 4
  | otherwise = Nothing

count values = Map.fromListWith (+) [(x, 1) | x <- values]
