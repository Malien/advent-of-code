{-# LANGUAGE QuasiQuotes #-}
import           Text.RawString.QQ (r)
import           Text.Regex.PCRE ((=~))

main = readFile "in" >>= print . process

test = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

process = eval Enabled 0 . parse

parse =
  map parseCommand .
  (=~ [r|do\(\)|don't\(\)|mul\((\d{1,3}),(\d{1,3})\)|])

data Command = Mul Int Int | Do | Dont deriving Show

parseCommand ["do()", "", ""] = Do
parseCommand ["don't()", "", ""] = Dont
parseCommand ['m':'u':'l':_, a, b] = Mul (read a) (read b)
parseCommand x = error $ "parseCommand: " ++ show x

data State = Enabled | Disabled

eval Enabled acc ((Mul a b):xs)  = eval Enabled (acc + a * b) xs
eval Disabled acc ((Mul _ _):xs) = eval Disabled acc xs
eval state acc (Do:xs)           = eval Enabled acc xs
eval state acc (Dont:xs)         = eval Disabled acc xs
eval _ acc []                    = acc
