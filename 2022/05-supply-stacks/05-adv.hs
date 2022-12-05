{-# LANGUAGE NamedFieldPuns #-}
import           Data.Array
import           Data.Array.ST
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Text.Regex.PCRE

main = process <$> readFile "in" >>= print

test =
  "    [D]    \n\
  \[N] [C]    \n\
  \[Z] [M] [P]\n\
  \ 1   2   3 \n\
  \\n\
  \move 1 from 2 to 1\n\
  \move 3 from 1 to 3\n\
  \move 2 from 2 to 1\n\
  \move 1 from 1 to 2"

data Move = Move {
  count :: Int,
  from  :: Int,
  to    :: Int
} deriving Show

parseProblem def = (parseStack stack, map parseMove $ lines moves)
  where [stack, moves] = splitOn "\n\n" def

parseMove line = Move { from, count, to }
  where [count, from, to] = map read $ getAllTextMatches (line =~ "\\d+")

parseStack =
    boundlessArray
  . map catMaybes
  . transpose
  . map (parseStackLine . (++" "))
  . init
  . lines

boundlessArray list = listArray (1, length list) list

parseStackLine []                     = []
parseStackLine ('[':c:']':' ':rest)   = Just c : parseStackLine rest
parseStackLine (' ':' ':' ':' ':rest) = Nothing : parseStackLine rest

makeMove stacks (Move { count, from, to }) = stacks // [(from, left), (to, movedOnto)]
  where (moved, left) = splitAt count $ stacks ! from
        movedOnto     = moved ++ stacks ! to

process =
    map head
  . elems
  . (uncurry $ foldl makeMove)
  . parseProblem
