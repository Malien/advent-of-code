import           Data.Array        (elems, listArray, (//))
import           Data.List.Split   (chunksOf)
import           Text.Regex.PCRE   (AllTextMatches (getAllTextMatches), (=~))

main = do
  input <- readFile "in"
  let robots = map parse $ lines input
  main' (concatMap splat interestingPlaces) robots

main' (iteration:rest) robots = do
  putStr $ process 103 101 iteration robots
  getLine
  main' rest robots

-- 53, 98 is the first iteration where I found patterns emerging in my input
interestingPlaces = iterate (^+^ (103, 101)) (53, 98)

splat (a, b) = [a, b]

process :: Int -> Int -> Int -> [Robot] -> String
process height width iteration robots = indexed (display height width . positions $ iteration) iteration
  where positions iteration = map (simulate height width iteration) robots
        indexed map idx = "\n\n" ++ show idx ++ "\n" ++ map

data Robot = Robot { pos :: (Int, Int), vel :: (Int, Int) } deriving (Show)

parse line = Robot (y, x) (vy, vx)
  where [x, y, vx, vy] = map read $ getAllTextMatches $ line =~ "-?\\d+"

simulate height width times (Robot pos vel) = (wrap y height, wrap x width)
  where (y, x) = (vel ^* times) ^+^ pos

wrap x limit = ((x `mod` limit) + limit) `mod` limit

(a, b) ^+^ (c, d) = (a + c, b + d)
(a, b) ^* c = (a * c, b * c)

display height width positions = unlines $ chunksOf width $ elems drawnArray
  where arr = listArray ((0, 0), (height - 1, width - 1)) $ repeat ' '
        drawnArray = arr // [(pos, '#') | pos <- positions]
