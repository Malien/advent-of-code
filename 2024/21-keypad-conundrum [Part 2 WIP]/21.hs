{-# LANGUAGE QuasiQuotes #-}

import           Data.Char         (digitToInt)
import           Data.Sequence     (Seq ((:<|), (:|>)), (<|), (><), (|>))
import qualified Data.Sequence     as Seq
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Text.RawString.QQ (r)

main = readFile "in" >>= print . process

test = tail [r|
029A
980A
179A
456A
379A
|]

process = sum . map cost . lines

-- foo = ["numpadBfs " ++ show a ++ " " ++ show b ++ " = " ++ show (numpadBfs a b) | a <- [minBound..maxBound], b <- [minBound..maxBound], a /= b]

cost row
  = (read (init row) *)
  . minimum
  . map length
  . concatMap keypadSeqPaths
  . concatMap keypadSeqPaths
  . numpadSeqPaths
  . map parseNumpadKey
  $ row

shortest paths = filter ((== minimum (map length paths)) . length) paths

numpadSeqPaths xs = numpadSeqPaths' [[]] (NumpadA:xs)
numpadSeqPaths' :: [[KeypadKey]] -> [NumpadKey] -> [[KeypadKey]]
numpadSeqPaths' acc [_] = acc
numpadSeqPaths' acc (from:to:rest) = numpadSeqPaths' [prev ++ next ++ [KeypadA] | prev <- acc, next <- numpadBfs from to] (to:rest)

keypadSeqPaths xs = keypadSeqPaths' [[]] (KeypadA:xs)
keypadSeqPaths' acc [_] = acc
keypadSeqPaths' acc (from:to:rest) = keypadSeqPaths' [prev ++ next ++ [KeypadA] | prev <- acc, next <- keypadBfs from to] (to:rest)

data NumpadKey = DigitKey Int | NumpadA deriving (Eq, Ord)
data KeypadKey = U | D | L | R | KeypadA deriving (Eq, Ord, Enum, Bounded)

parseNumpadKey 'A' = NumpadA
parseNumpadKey c   = DigitKey $ digitToInt c

parseKeypadKey 'A' = KeypadA
parseKeypadKey '^' = U
parseKeypadKey '>' = R
parseKeypadKey '<' = L
parseKeypadKey 'v' = D

numpadNeighbors  NumpadA     = [(L, DigitKey 0), (U, DigitKey 3)]
numpadNeighbors (DigitKey 0) = [(U, DigitKey 2), (R, NumpadA)]
numpadNeighbors (DigitKey 1) = [(U, DigitKey 4), (R, DigitKey 2)]
numpadNeighbors (DigitKey 2) = [(L, DigitKey 1), (U, DigitKey 5), (R, DigitKey 3), (D, DigitKey 0)]
numpadNeighbors (DigitKey 3) = [(U, DigitKey 6), (L, DigitKey 2), (D, NumpadA)]
numpadNeighbors (DigitKey 4) = [(U, DigitKey 7), (R, DigitKey 5), (D, DigitKey 1)]
numpadNeighbors (DigitKey 5) = [(U, DigitKey 8), (L, DigitKey 4), (R, DigitKey 6), (D, DigitKey 2)]
numpadNeighbors (DigitKey 6) = [(U, DigitKey 9), (L, DigitKey 5), (D, DigitKey 3)]
numpadNeighbors (DigitKey 7) = [(R, DigitKey 8), (D, DigitKey 4)]
numpadNeighbors (DigitKey 8) = [(L, DigitKey 7), (R, DigitKey 9), (D, DigitKey 5)]
numpadNeighbors (DigitKey 9) = [(L, DigitKey 8), (D, DigitKey 6)]

numpadBfs start target | start == target = [[]]
numpadBfs start target = numpadBfs' Set.empty [] target neighbors
  where neighbors = Seq.fromList [(neighbor, [direction]) | (direction, neighbor) <- numpadNeighbors start]

numpadBfs'
  :: Set (NumpadKey, KeypadKey)
  -> [[KeypadKey]]
  -> NumpadKey
  -> Seq (NumpadKey, [KeypadKey])
  -> [[KeypadKey]]
numpadBfs' visited [] target ((pos, path):<|rest)
  | pos == target = numpadBfs' visited [path] target rest
  | (pos, head path) `Set.member` visited = numpadBfs' visited [] target rest
  | otherwise = numpadBfs' visited' [] target rest'
  where
    visited' = Set.insert (pos, head path) visited
    neighbors = [(neighbor, direction:path) | (direction, neighbor) <- numpadNeighbors pos]
    rest' = rest >< Seq.fromList neighbors

numpadBfs' visited acc@(shortestPath:_) target ((pos, path):<|rest)
  | length path > length shortestPath = numpadBfs' visited' acc target rest
  | pos == target = numpadBfs' visited (path:acc) target rest
  | (pos, head path) `Set.member` visited = numpadBfs' visited acc target rest
  | otherwise = numpadBfs' visited' acc target rest'
  where
    visited' = Set.insert (pos, head path) visited
    neighbors = [(neighbor, direction:path) | (direction, neighbor) <- numpadNeighbors pos]
    rest' = rest >< Seq.fromList neighbors

numpadBfs' visited acc target Seq.Empty = map reverse acc

keypadNeighbors KeypadA = [(D, R), (L, U)]
keypadNeighbors U       = [(D, D), (D, KeypadA)]
keypadNeighbors L       = [(R, D)]
keypadNeighbors D       = [(L, L), (U, U), (R, R)]
keypadNeighbors R       = [(U, KeypadA), (L, D)]

inverse U = D
inverse D = U
inverse L = R
inverse R = L

keypadBfs a b       | a == b = [[]]
keypadBfs KeypadA U = [[L]]
keypadBfs KeypadA D = [[D, L], [L, D]]
keypadBfs KeypadA L = [[L, D, L], [D, L, L]]
keypadBfs KeypadA R = [[D]]

keypadBfs U D       = [[D]]
keypadBfs U L       = [[D, L]]
keypadBfs U R       = [[D, R], [R, D]]

keypadBfs D L       = [[L]]
keypadBfs D R       = [[R]]

keypadBfs R L       = [[L, L]]

keypadBfs from to   = map (reverse . map inverse) $ keypadBfs to from

instance Show NumpadKey where
  show (DigitKey n) = show n
  show NumpadA      = "A"

instance Show KeypadKey where
  show U       = "^"
  show D       = "v"
  show L       = "<"
  show R       = ">"
  show KeypadA = "A"
