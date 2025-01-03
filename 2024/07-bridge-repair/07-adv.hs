{-# LANGUAGE QuasiQuotes #-}
import           Text.RawString.QQ

main = readFile "in" >>= print . process

test = tail [r|
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
|]

process = sum . map fst . filter (uncurry eval1) . map parse . lines

parse :: String -> (Int, [Int])
parse line = (read $ init result, map read rest)
  where (result:rest) = words line

eval1 target (x:xs) = eval x target xs
eval acc target (x:xs) | acc > target = False
eval acc target (x:xs) =
  eval (acc + x) target xs ||
  eval (acc * x) target xs ||
  eval (acc `concatDigits` x) target xs
eval acc target [] = acc == target

-- Optimizations for the vast majority of cases. Branch predictor works here better, than calculating log10
concatDigits a b | b < 10 = 10 * a + b
                 | b < 100 = 100 * a + b
                 | b < 1000 = 1000 * a + b
concatDigits a b = 10 ^ ceiling (logBase 10 (fromIntegral b)) * a + b
