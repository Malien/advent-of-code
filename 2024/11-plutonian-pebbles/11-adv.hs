import           Data.Map          (Map)
import qualified Data.Map          as Map

main = readFile "in" >>= print . process

test = "125 17"

process = sum . map (fst . blink Map.empty 75 . read) . words

blink :: Map (Int, Int) Int -> Int -> Int -> (Int, Map (Int, Int) Int)
blink cache 0 x = (1, cache)
blink cache times x = case Map.lookup (x, times) cache of
  (Just cached) -> (cached, cache)
  Nothing -> (count, Map.insert (x, times) count nextCache)
  where (count, nextCache) = case blinkOnce x of
          [next] -> blink cache (times - 1) next
          [left, right] -> let
            (leftCount, cache') = blink cache (times - 1) left
            (rightCount, cache'') = blink cache' (times - 1) right
            in (leftCount + rightCount, cache'')

blinkOnce :: Int -> [Int]
blinkOnce 0 = [1]
blinkOnce x | even digits = [x `div` divisor, x `mod` divisor]
  where (digits, divisor) = digitCount x
blinkOnce x = [x * 2024]

digitCount x
  | x < 10 = (1, 1)
  | x < 100 = (2, 10)
  | x < 1000 = (3, 1)
  | x < 10000 = (4, 100)
  | x < 100000 = (5, 1)
  | x < 1000000 = (6, 1000)
  | x < 10000000 = (7, 1)
  | x < 100000000 = (8, 10000)
  | otherwise = (digits, 10 ^ (digits `div` 2))
  where digits = ceiling $ logBase 10 $ fromIntegral x + 1
