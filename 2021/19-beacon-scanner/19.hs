import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Data.Maybe

main = compute "test.txt" >>= print
-- main = $ correlate s0 s1

compute file = solution <$> readFile file

-- solution str = Set.size $ resolve Set.empty [((0, 0, 0), sensor)] sensors
    -- where (sensor:sensors) = map (Set.fromList . parseInput) $ splitOn "\n\n" str
solution str = map (snd) $ correlate s1 s4
    -- where (sensor:sensors) = map (Set.fromList . parseInput) $ splitOn "\n\n" str
    where [s0,s1,s2,s3,s4] = map (Set.fromList . parseInput) $ splitOn "\n\n" str

type Point = (Int, Int, Int)

data ResolutionResult = Resolved Point (Set Point) | Unresolved (Set Point) deriving Show

-- resolve totalPoints [] [] = totalPoints
-- resolve _ [] _ = error "y tho?"
-- resolve totalPoints (resolved:other) unresolved = trace ("Resolved " ++ (show $ length newResolved)) $ resolve newPoints (other ++ newResolved) newUnresolved
    -- where (newResolved, newUnresolved) = collectResolutions $ map (resolutionCycle resolved) unresolved
          -- newPoints = Set.union totalPoints (uncurry relativeTo $ resolved)

-- collectResolutions = collectResolutions' [] []
-- collectResolutions' resolved unresolved [] = (resolved, unresolved)
-- collectResolutions' resolved unresolved ((Resolved point set):rest) = 
    -- collectResolutions' ((point, set):resolved) unresolved rest
-- collectResolutions' resolved unresolved ((Unresolved set):rest) = 
    -- collectResolutions' resolved (set:unresolved) rest

-- resolutionCycle (point, setA) setB = case res of
        -- (Just (p, newB)) -> trace ("Resolved " ++ show (addPoint point p)) $ Resolved (addPoint point p) newB
        -- (Nothing) -> trace ("Uneresolved") $ Unresolved setB
    -- where res = listToMaybe $ correlate setA setB

axis = [
    (1,1,1),
    (1,1,-1),
    (1,-1,1),
    (1,-1,-1),
    (-1,1,1),
    (-1,1,-1),
    (-1,-1,1),
    (-1,-1,-1)]

applyTransform (a,b,c) (x,y,z) = (a * x, b * y, c * z)
neg (a,b,c) = (-a,-b,-c)
addPoint (a,b,c) (x,y,z) = (a+x, b+y, c+z)

relativeTo :: Point -> Set Point -> Set Point
relativeTo (dx, dy, dz) = Set.map (\(x, y, z) -> (x - dx, y - dy, z - dz))

occurrances a b original set = Set.size $ Set.intersection (relativeTo a original) (relativeTo b set)

validCorrelation pa pb setA setB | inCommon >= 2 = Just $ (addPoint (neg pb) pa, inCommon)
                                 | otherwise      = Nothing
    where transposedA = relativeTo pa setA
          transposedB = relativeTo pb setB
          inCommon = Set.size $ Set.intersection transposedA transposedB

correlate setA setB = 
    concatMap (\bRotation -> 
        concatMap (\pa -> 
            mapMaybe (\pb -> 
                validCorrelation pa pb setA bRotation
            ) (Set.toList bRotation)
        ) (Set.toList setA)
    ) (rotations setB)

rotations :: Set Point -> [Set Point]
rotations set = map (\transform -> Set.map (applyTransform transform) set) axis 

parseInput = map parsePoint . tail . lines

parsePoint :: String -> (Int, Int, Int)
parsePoint str = (x, y, z)
    where [x, y, z] = map read $ splitOn "," str

