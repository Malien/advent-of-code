import           Data.Char (digitToInt, intToDigit, isDigit)
import           Data.List (isPrefixOf, findIndex)

test = "\
\two1nine\n\
\eightwothree\n\
\abcone2threexyz\n\
\xtwone3four\n\
\4nineeightseven2\n\
\zoneight234\n\
\7pqrstsixteen"

main = readFile "/Users/yaroslav/Downloads/input.txt" >>= putStr . process

process = unlines . map (show . getNum . extractDigits) . lines

getNum list = last list * 10 + head list

spelledDigits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

extractDigits = extractDigits' []
extractDigits' acc "" = acc
extractDigits' acc str@(first:rest) = case (writtenDigit, spelledDigit) of
    (Nothing, Just x)  -> extractDigits' (x:acc) rest
    (Just x, Nothing)  -> extractDigits' (x:acc) rest
    (Nothing, Nothing) -> extractDigits' acc rest
    where writtenDigit = if isDigit first then Just (digitToInt first) else Nothing
          spelledDigit = (+1) <$> findIndex (`isPrefixOf` str) spelledDigits
