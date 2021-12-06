import Control.Lens
import Data.List.Lens
import Data.List.Split

main = readFile "input.txt" >>= print . solution

solution = totalFish . (!!256) . progression . initialFish . map read . splitOn ","

initialFish init = foldl (flip addFish) (take 7 $ repeat 0) init

addFish f = element f %~ (+1)

iteration ((x:xs), (w:ws)) = (xs ++ [x + w], ws ++ [x])

progression initial = iterate iteration (initial, [0, 0])

totalFish (fish, waiting) = sum (fish ++ waiting)
