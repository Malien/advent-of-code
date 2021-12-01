
main = readFile "input.txt" >>= print . walkForest (3, 1) . lines

walkForest (_, y) forest | y >= length forest = 0
walkForest pos forest = (countTree $ treeAt pos $ forest) + walkForest nextPos forest
    where nextPos = (fst pos + 3, snd pos + 1)

countTree '#' = 1
countTree '.' = 0

treeAt (x, y) forest = row !! (x `mod` length row)
    where row = forest !! y

