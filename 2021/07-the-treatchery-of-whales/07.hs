import Data.List.Split
import qualified Data.Map as Map
import Data.Map (Map)

main = readFile "test.txt" >>= print . solution . map read . splitOn ","

solution xs = minimum $ crabRave range crabs
    where crabs = combineCrabs xs
          range = crabRange $ Map.keys crabs

combineCrabs :: [Int] -> Map Int Int
combineCrabs = foldl (\map key -> Map.insertWith (+) key 1 map) Map.empty

crabRange crabs = [minimum(crabs)..maximum(crabs)]

crabRave range crabs = map (migrationCost crabs) range

migrationCost :: Map Int Int -> Int -> Int
migrationCost crabs to = sum . map (movingCost to) . Map.toList $ crabs

movingCost :: Int -> (Int, Int) -> Int
movingCost to (position, count) = count * (algSum $ abs $ position - to)

algSum x = (x + 1) * x `div` 2

