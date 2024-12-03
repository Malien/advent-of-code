{-# LANGUAGE QuasiQuotes #-}

import           Text.RawString.QQ (r)
import           Text.Regex.PCRE   ((=~))

main = readFile "in" >>= print . process

test = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

process = sum . map (uncurry (*)) . parse

parse = map parseNum . (=~ [r|mul\((\d{1,3}),(\d{1,3})\)|])

parseNum [_, a, b] = (read a, read b)
