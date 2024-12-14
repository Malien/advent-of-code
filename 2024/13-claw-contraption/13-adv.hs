{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes         #-}

import           Data.List.Split   (splitOn)
import           Data.Maybe        (mapMaybe)
import           Text.RawString.QQ (r)
import           Text.Regex.PCRE   ((=~))

main = readFile "in" >>= print . process

test = tail [r|
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
|]

process = sum . map cost . mapMaybe (equasion . parse) . splitOn "\n\n"

cost (a, b) = a * 3 + b

data PrizeMachine = PrizeMachine {
  buttonA :: (Int, Int),
  buttonB :: (Int, Int),
  prize   :: (Int, Int)
} deriving Show

parse block = PrizeMachine {
  buttonA = (read x, read y),
  buttonB = (read x', read y'),
  prize   = (read x'' + 10000000000000, read y'' + 10000000000000)
} where [a, b, prize] = lines block
        [[_, x, y]] = a =~ "X\\+(\\d+), Y\\+(\\d+)" :: [[String]]
        [[_, x', y']] = b =~ "X\\+(\\d+), Y\\+(\\d+)" :: [[String]]
        [[_, x'', y'']] = prize =~ "X=(\\d+), Y=(\\d+)" :: [[String]]

aeq (PrizeMachine { buttonA = (xa, ya), buttonB = (xb, yb), prize = (x, y) })
  | ma == 0 = Just da
  | otherwise = Nothing
  where (da, ma) = (y * xb - x * yb) `divMod` (ya * xb - yb * xa)

beq (PrizeMachine { buttonA = (xa, ya), buttonB = (xb, yb), prize = (x, y) }) a
  | mb == 0 = Just db
  | otherwise = Nothing
  where (db, mb) = (x - xa * a) `divMod` xb

equasion machine = do
  a <- aeq machine
  b <- beq machine a
  Just (a, b)
