import Data.Bits
import Data.Char

main = readFile "input.txt" >>= print . solution . lines

solution :: [String] -> Int
solution xs = gamma total counts * epsilon total counts
    where counts = countAllBits xs
          total = length xs

countAllBits = foldl (zipWith (+)) (repeat 0) . map (map digitToInt)

gamma total = foldl (\acc count -> (acc `shift` 1) + (mostCommon count total)) 0
epsilon total = foldl (\acc count -> (acc `shift` 1) + (leastCommon count total)) 0

mostCommon count total | count >= total - count = 1
                       | otherwise              = 0

leastCommon count total | count < total - count = 1
                        | otherwise             = 0

