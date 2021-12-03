import Data.Bits
import Data.Char
import Numeric

main = readFile "input.txt" >>= print . solution . lines

solution xs = oxygen xs 0 * co2 xs 0

oxygen [x] _ = readBin x
oxygen xs n = oxygen (filter (hasBitAt n bit) xs) (n + 1)
    where count = countBits n xs
          bit = mostCommon count (length xs)

co2 [x] _ = readBin x
co2 xs n = co2 (filter (hasBitAt n bit) xs) (n + 1)
    where count = countBits n xs
          bit = leastCommon count (length xs)

hasBitAt n bit bitstr = bitstr !! n == intToDigit bit

countAllBits xs = map (`countBits` xs) [0..(length (head xs)) - 1]

countBits idx = length . filter (== '1'). map (!! idx)

readBin = fst . head . readInt 2 (`elem` "01") digitToInt

mostCommon count total | count >= total - count = 1
                       | otherwise              = 0

leastCommon count total | count < total - count = 1
                        | otherwise             = 0

