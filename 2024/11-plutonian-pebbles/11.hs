main = readFile "in" >>= print . process

test = "125 17"

process = length . (!!25) . iterate (concatMap blink) . map read . words

blink :: Int -> [Int]
blink 0 = [1]
blink x | even (digitCount x) = [x `div` 10 ^ (digitCount x `div` 2), x `mod` 10 ^ (digitCount x `div` 2)]
blink x = [x * 2024]

digitCount x = ceiling $ logBase 10 $ fromIntegral x + 1

