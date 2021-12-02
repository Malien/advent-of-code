import Data.List.Split
import Data.Text (isSuffixOf, pack)
import Data.Char
import Debug.Trace

main = readFile "input.txt" >>= print . length . filter validPassport . splitOn "\n\n"

required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

fields = map (fieldPair) . words

fieldPair str = (key, value)
    where [key, value] = splitOn ":" str

validField "byr" value = validYear 1920 2002 value
validField "iyr" value = validYear 2010 2020 value
validField "eyr" value = validYear 2020 2030 value
validField "hgt" value = validHeight "cm" 150 193 value || validHeight "in" 59 76 value
validField "hcl" ('#':color) = length color == 6 && all isHexDigit color
validField "ecl" value = value `elem` eyeColors
validField "pid" value = length value == 9 && all isDigit value
validField "cid" _ = True
validField _ _ = False

validHeight prefix from to value = 
    (pack prefix) `isSuffixOf` (pack value) && num >= from && num <= to
    where num = read $ take (length value - length prefix) value
validYear from to year = length year == 4 && num >= from && num <= to
    where num = read year

eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validPassport str = all (`elem` (map fst passport)) required && all (uncurry validField) passport
    where passport = fields str

