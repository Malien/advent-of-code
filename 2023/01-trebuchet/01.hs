import           Data.Char (digitToInt, isDigit)

test = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"

main = readFile "input.txt" >>= print . process

process = sum . map (getNum . map digitToInt . filter isDigit) . lines

getNum list = head list * 10 + last list
