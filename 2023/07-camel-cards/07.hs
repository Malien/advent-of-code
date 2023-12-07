import           Data.Array
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe
import           Data.Ord
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Debug.Trace
import           Text.Regex.PCRE

main = readFile "in" >>= print . process

test = "\
\32T3K 765 \n\
\T55J5 684 \n\
\KK677 28  \n\
\KTJJT 220 \n\
\QQQJA 483"

process = sum . zipWith (*) [1..] . map snd . sortOn fst . map parseBid . lines

data Card =
    Ace | King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two
    deriving (Eq, Ord, Enum, Bounded, Ix)

instance Show Card where
    show Ace   = "A"
    show King  = "K"
    show Queen = "Q"
    show Jack  = "J"
    show Ten   = "T"
    show Nine  = "9"
    show Eight = "8"
    show Seven = "7"
    show Six   = "6"
    show Five  = "5"
    show Four  = "4"
    show Three = "3"
    show Two   = "2"

parseCard 'A' = Ace
parseCard 'K' = King
parseCard 'Q' = Queen
parseCard 'J' = Jack
parseCard 'T' = Ten
parseCard '9' = Nine
parseCard '8' = Eight
parseCard '7' = Seven
parseCard '6' = Six
parseCard '5' = Five
parseCard '4' = Four
parseCard '3' = Three
parseCard '2' = Two

parseBid inp = (parseHand hand, read big :: Int)
    where [hand, big] = words inp

parseHand inp = Hand $ map parseCard inp

newtype Hand = Hand [Card] deriving (Eq)

instance Show Hand where
    show (Hand cards) = concatMap show cards

instance Ord Hand where
    compare a@(Hand aCards) b@(Hand bCards)
        | aRuleRank == bRuleRank = Down aCards `compare` Down bCards
        | otherwise              = Down aRuleRank `compare` Down bRuleRank
        where aRuleRank = matchRules a
              bRuleRank = matchRules b

fiveOfAKind = elem 5 . elems . countCards

fourOfAKind = elem 4 . elems . countCards

fullHouse hand = case (pairs, threes) of
    ([a], [b]) -> True
    _          -> False
    where pairs = filter (==2) $ elems $ countCards hand
          threes = filter (==3) $ elems $ countCards hand

threeOfAKind = elem 3 . elems . countCards

twoPairs hand = case pairs of
    [a, b] -> True
    _      -> False
    where pairs = filter (==2) $ elems $ countCards hand

onePair = elem 2 . elems . countCards 

highCard _hand = True

emptyHandCount = listArray (Ace, Two) (repeat 0)
countCards (Hand cards) = accum (+) emptyHandCount $ map (, 1) cards

rules :: [Hand -> Bool]
rules = [fiveOfAKind, fourOfAKind, fullHouse, threeOfAKind, twoPairs, onePair, highCard]

matchRules hand = fromJust $ findIndex (\rule -> rule hand) rules

applyWinnings = zipWith (,) [1..]
