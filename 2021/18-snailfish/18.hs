import Debug.Trace
import Data.Char
import Data.List
import Data.Maybe

main = compute "test.txt" >>= print

compute file = solution <$> readFile file
-- compute file = readFile file >>= mapM print . solution

solution = largestSum . map parseInput . lines

cartiseanProduct xs ys = [(x, y) | x <- xs, y <- ys]

largestSum xs = 
    maximum . 
    map (magnitude . reduce . uncurry addSnums) .
    filter (uncurry (/=)) $
    cartiseanProduct xs xs

sumSnums :: [SNum] -> SNum
sumSnums = foldl1 (curry $ reduce . uncurry addSnums)

-- SnailfishNumber
data SNum = Pair SNum SNum | Regular Int deriving Eq

instance Show SNum where
    show (Pair lhs rhs) = "[" ++ show lhs ++ "," ++ show rhs ++ "]"
    show (Regular num) = show num

parseInput str = res
    where (res, "") = parseInput' str

parseInput' ('[':rest) = (Pair lhs rhs, tail left2)
    where (lhs, left1) = parseInput' rest
          (rhs, left2) = parseInput' (tail left1)
parseInput' str = (Regular $ read before, after)
    where before = takeWhile isDigit str
          after = dropWhile isDigit str

addSnums = Pair

data ExplosionState = Continue
                    | Propagate Int Int
                    | PropagateLeft Int
                    | PropagateRight Int
                    | Break
                    deriving Show

isContinue Continue = True
isContinue _ = False

data Explosion = Explosion SNum ExplosionState deriving Show

explosionNum (Explosion x _) = x
explosionState (Explosion _ state) = state

explode = explosionNum . fromJust . find (isContinue . explosionState) . iterate (explode' 0 . explosionNum) . (`Explosion` Break)
explode' depth (Pair (Regular lhs) (Regular rhs)) | depth >= 4 = 
    Explosion (Regular 0) (Propagate lhs rhs)
explode' depth (Pair lhs rhs) = explodeLeft (depth + 1) newLhs rhs state 
    where (Explosion newLhs state) = explode' (depth + 1) lhs
explode' _ n = Explosion n Continue

explodeLeft depth lhs rhs Continue = explodeRight lhs newRhs state
    where (Explosion newRhs state) = explode' depth rhs
explodeLeft _ lhs rhs (PropagateLeft addL) = Explosion (Pair lhs rhs) (PropagateLeft addL)
explodeLeft _ lhs rhs (PropagateRight addR) = Explosion (Pair lhs (addLeft rhs addR)) Break
explodeLeft _ lhs rhs (Propagate addL addR) = Explosion (Pair lhs (addLeft rhs addR)) (PropagateLeft addL)
explodeLeft _ lhs rhs Break = Explosion (Pair lhs rhs) Break

explodeRight lhs rhs Continue = Explosion (Pair lhs rhs) Continue 
explodeRight lhs rhs (PropagateLeft addL) = Explosion (Pair (addRight lhs addL) rhs) Break
explodeRight lhs rhs (PropagateRight addR) = Explosion (Pair lhs rhs) (PropagateRight addR)
explodeRight lhs rhs (Propagate addL addR) = 
    Explosion (Pair (addRight lhs addL) rhs) (PropagateRight addR)
explodeRight lhs rhs Break = Explosion (Pair lhs rhs) Break 

addLeft (Regular num) amount = Regular $ num + amount
addLeft (Pair lhs rhs) amount = Pair (addLeft lhs amount) rhs

addRight (Regular num) amount = Regular $ num + amount
addRight (Pair lhs rhs) amount = Pair lhs (addRight rhs amount)

data SplitState = ContinueS | BreakS
data Split = Split SNum SplitState

split (Regular num) | num > 9 = Split pair BreakS
    where pair = Pair (Regular $ num `div` 2) (Regular $ (num `div` 2) + (num `mod` 2))
split r@(Regular _) = Split r ContinueS
split (Pair lhs rhs) = splitLeft newLhs rhs state
    where (Split newLhs state) = split lhs

splitLeft lhs rhs ContinueS = splitRight lhs newRhs state
    where (Split newRhs state) = split rhs
splitLeft lhs rhs BreakS = Split (Pair lhs rhs) BreakS

splitRight lhs rhs ContinueS = Split (Pair lhs rhs) ContinueS
splitRight lhs rhs BreakS = Split (Pair lhs rhs) BreakS

data ReductionState = Unprocessed | Reduced | Splitted

class ToReductionState a where
    toReduction :: a -> ReductionState

instance ToReductionState ExplosionState where
    toReduction Continue = Reduced
    toReduction _ = Unprocessed

instance ToReductionState SplitState where
    toReduction ContinueS = Splitted
    toReduction BreakS = Unprocessed

reduce = reduce' Unprocessed
reduce' Splitted snum = snum
reduce' Reduced snum = reduce' (toReduction state) newSnum
    where (Split newSnum state) = split snum
reduce' Unprocessed snum = reduce' (toReduction state) newSnum
    where (Explosion newSnum state) = explode' 0 snum

magnitude (Regular num) = num
magnitude (Pair lhs rhs) = 3 * magnitude lhs + 2 * magnitude rhs

