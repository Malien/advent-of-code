import           Data.Array      (Array, assocs, listArray, (!), (//))
import           Data.Char       (ord)
import           Data.List       (isSuffixOf)
import           Data.List.Split (splitOn)

main = readFile "in" >>= print . process

test1 = "HASH"
test = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

process = sum . lensPower . foldl applyCommand emptyMap . map parseCommand . splitOn "," . filter (/= '\n')

hash = foldl (\acc v -> ((acc + v) * 17) `mod` 256) 0 . map ord

data Command = Remove String | Replace String Int deriving Show

parseCommand str | "-" `isSuffixOf` str = Remove $ init str
                 | otherwise = let (a,b) = break (== '=') str in Replace a (read $ tail b)

type HASHMAP = Array Int [(String, Int)]

emptyMap = listArray (0,255) $ repeat []

replace map label value = map // [(h, res)]
    where h = hash label
          res = replaceSingle label value $ map ! h

remove map label = map // [(h, res)]
    where h = hash label
          res = filter ((/= label) . fst) $ map ! h

replaceSingle label value ((l, v):xs)
    | l == label = (l, value) : xs
    | otherwise  = (l, v) : replaceSingle label value xs
replaceSingle label value [] = [(label, value)]

applyCommand map (Replace label value) = replace map label value
applyCommand map (Remove label)        = remove map label

-- lensPower :: HASHMAP -> [Int]
lensPower m = concatMap (uncurry boxPower) $ assocs m

-- boxPower :: Int -> [(String, Int)] -> [Int]
boxPower idx lenses = map (* (idx + 1)) $ zipWith (*) [1..] $ map snd lenses
