{-# LANGUAGE QuasiQuotes #-}

import           Data.Array
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Maybe
import           Data.Ord
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Debug.Trace
import           Text.RawString.QQ (r)
import           Text.Regex.PCRE

main = readFile "in" >>= print . process

test = tail [r|
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
L232
|]

parse ('L':rest) = - read rest
parse ('R':rest) = read rest

apply (clicks, _) change
  | clicks == 0 && change < 0 = (m, d - 1)
  | m == 0      = (m, d + 1)
  | next < 0    = (m, d)
  | otherwise   = (m, d)
  where next = clicks + change
        m = next `mod` 100
        d = abs $ next `div` 100



process = scanl apply (50, 0) . map parse . lines

-- 2216 is too low
-- 6173 is too high
