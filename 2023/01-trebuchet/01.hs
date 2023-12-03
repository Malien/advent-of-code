import           Data.Char (digitToInt, isDigit)

test = "\
\1abc2\n\
\pqr3stu8vwx\n\
\a1b2c3d4e5f\n\
\treb7uchet"

main = readFile "input.txt" >>= print . process

process = sum . map (getNum . map digitToInt . filter isDigit) . lines

getNum list = head list * 10 + last list
