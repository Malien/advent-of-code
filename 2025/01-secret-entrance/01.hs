{-# LANGUAGE QuasiQuotes #-}

import           Data.List
import           Data.List.Split
import           Data.Ord
import           Data.Char
import           Data.Maybe
import           Debug.Trace
import           Text.Regex.PCRE
import           Data.Array
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Text.RawString.QQ (r)

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
|]

parse ('L':rest) = - read rest
parse ('R':rest) = read rest

apply clicks change = (clicks + change + 100) `mod` 100

process = length . filter (==0) . scanl apply 50 . map parse . lines

