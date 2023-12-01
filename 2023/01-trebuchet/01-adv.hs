import           Data.Char (digitToInt, intToDigit, isDigit)
import           Data.List (isPrefixOf)

test = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"

main = readFile "input.txt" >>= print . process

process = sum . map (getNum . (`extractDigits` [])) . lines

getNum list = last list * 10 + head list

spelledDigits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

firstTrueIdx list = firstTrueIdx' list 1
    where firstTrueIdx' [] _          = Nothing
          firstTrueIdx' (True:xs) idx = Just idx
          firstTrueIdx' (_:xs) idx    = firstTrueIdx' xs (idx + 1)

extractDigits "" acc = acc
extractDigits str@(first:rest) acc = case (writtenDigit, firstTrueIdx spelledMask) of
    (Nothing, Just x)  -> extractDigits rest (x:acc)
    (Just x, Nothing)  -> extractDigits rest (x:acc)
    (Nothing, Nothing) -> extractDigits rest acc
    where spelledMask = map (`isPrefixOf` str) spelledDigits
          writtenDigit = if isDigit first then Just (digitToInt first) else Nothing
