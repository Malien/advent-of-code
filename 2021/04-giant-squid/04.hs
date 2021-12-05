import Data.List.Split

main = readFile "input.txt" >>= print . solution . splitOn "\n\n"

solution (nums:boards) = solution' comboards bingoSeq
    where parsed = map parseBoard boards
          bingoSeq = map read $ splitOn "," nums
          comboards = zip (repeat []) parsed

solution' comboards (x:xs) = 
    if null playingBoards
    then x * (head scores)
    else solution' playingBoards xs
    where nextTurn = map (checkOff x) comboards
          playingBoards = filter (not . completed . fst) nextTurn
          scores = map (sum . map snd) . map snd $ nextTurn

checkOff num (comps, board) = (comps ++ newcomps, newboard)
    where (newcomps, newboard) = mark num board

parseBoard = 
    concatMap (\(i, row) -> map (\(j, num) -> ((i,j), num)) row) 
    . zip [0..] 
    . map (zip [0..] . map read . words) 
    . lines

mark num board = mark' num board [] []
mark' num [] comps board = (comps, board)
mark' num ((pos, v):xs) comps board | num == v = (pos: comps, board ++ xs)
mark' num (x@(pos, v):xs) comps board = mark' num xs comps (x:board)

completed comps = any (==5) (rows ++ cols)
    where foldfn (rows, cols) (i, j) = (updateList j (+1) rows, updateList i (+1) cols)
          (rows, cols) = foldl foldfn (init, init) comps
          init = take 5 $ repeat 0

updateList idx f = updateList' idx f . zip [0..]
updateList' 0 f ((_, x):xs) = f x : map snd xs
updateList' idx f (x:xs) = snd x : updateList' (idx - 1) f xs

