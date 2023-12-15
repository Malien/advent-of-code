import           Data.Char       (ord)
import           Data.List.Split (splitOn)

main = readFile "in" >>= print . process

test1 = "HASH"
test = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

process = sum . map hash . splitOn "," . filter (/= '\n')

hash = foldl (\acc v -> ((acc + v) * 17) `mod` 256) 0 . map ord
