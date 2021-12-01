import Data.List.Split

input = [
    "1-3 a: abcde",
    "1-3 b: cdefg",
    "2-9 c: ccccccccc"]

main = readFile "input.txt" >>= print . length . filter isValidPassword . lines
-- main = print . map isValidPassword $ input

isValidPassword line = matchesRule (parseRule rule) password
    where [rule, password] = splitOn ": " line

data Rule = Rule (Int, Int) Char

-- matchesRule (Rule (start, end) char) password = count >= start && count <= end
    -- where count = length . filter (== char) $ password

matchesRule (Rule (start, end) char) password = 
    (password !! (start-1) == char) /= (password !! (end-1) == char)

parseRule str = Rule (start, end) char
    where [range, [char]] = words str
          [start, end] = map read $ splitOn "-" range


