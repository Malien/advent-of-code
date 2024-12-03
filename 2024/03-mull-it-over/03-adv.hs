{-# LANGUAGE QuasiQuotes #-}
import           Data.Char
import           Text.RawString.QQ
import           Text.Regex.TDFA

main = readFile "in" >>= print . process

test = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

process = eval Enabled 0 . parse

parse =
  map parseCommand .
  getAllTextMatches .
  (=~ [r|do\(\)|don't\(\)|mul\([[:digit:]][[:digit:]]?[[:digit:]]?,[[:digit:]][[:digit:]]?[[:digit:]]?\)|])

data Command = Mul Int Int | Do | Dont deriving Show

parseCommand "do()" = Do
parseCommand "don't()" = Dont
parseCommand ('m':'u':'l':'(':xs) = Mul (read a) (read b)
  where (a, ',':rest) = break (==',') xs
        b = takeWhile isDigit rest

data State = Enabled | Disabled

eval Enabled acc ((Mul a b):xs)  = eval Enabled (acc + a * b) xs
eval Disabled acc ((Mul _ _):xs) = eval Disabled acc xs
eval state acc (Do:xs)           = eval Enabled acc xs
eval state acc (Dont:xs)         = eval Disabled acc xs
eval _ acc []                    = acc
