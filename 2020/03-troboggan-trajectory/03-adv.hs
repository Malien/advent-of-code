slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

main = readFile "input.txt" >>= print . product . walkSlopes slopes . lines

walkSlopes slopes forest = map (\slope -> walkForest slope slope forest) slopes

walkForest _ (_, y) forest | y >= length forest = 0
walkForest slope@(slopeX, slopeY) pos@(x, y) forest 
    = (countTree $ treeAt pos $ forest) + walkForest slope nextPos forest
    where nextPos = (x + slopeX, y + slopeY)

countTree '#' = 1
countTree '.' = 0

treeAt (x, y) forest = row !! (x `mod` length row)
    where row = forest !! y

