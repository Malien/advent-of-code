{-# LANGUAGE QuasiQuotes #-}

import           Data.Char
import           Text.RawString.QQ
import           Text.Regex.TDFA

main = readFile "in" >>= print . process

test = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

process = sum . map (uncurry (*)) . parse

parse :: String -> [(Int, Int)]
parse = 
  map parseNum . 
  getAllTextMatches .
  (=~ [r|mul\([[:digit:]][[:digit:]]?[[:digit:]]?,[[:digit:]][[:digit:]]?[[:digit:]]?\)|])

parseNum ('m':'u':'l':'(':xs) = (read a, read b)
  where (a, ',':rest) = break (==',') xs
        b = takeWhile isDigit rest

