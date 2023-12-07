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
    Ace | King | Queen | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two | Joker
    deriving (Eq, Ord, Enum, Bounded, Ix)

instance Show Card where
    show Ace   = "A"
    show King  = "K"
    show Queen = "Q"
    show Ten   = "T"
    show Nine  = "9"
    show Eight = "8"
    show Seven = "7"
    show Six   = "6"
    show Five  = "5"
    show Four  = "4"
    show Three = "3"
    show Two   = "2"
    show Joker = "J"

parseCard 'A' = Ace
parseCard 'K' = King
parseCard 'Q' = Queen
parseCard 'T' = Ten
parseCard '9' = Nine
parseCard '8' = Eight
parseCard '7' = Seven
parseCard '6' = Six
parseCard '5' = Five
parseCard '4' = Four
parseCard '3' = Three
parseCard '2' = Two
parseCard 'J' = Joker

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

fiveOfAKind hand = elem (5 - jokers) $ elems count
    where (count, jokers) = countCards hand

fourOfAKind hand = elem (4 - jokers) $ elems count
    where (count, jokers) = countCards hand

fullHouse hand = case (length pairs, length threes, jokers) of
    (1, 1, 0) -> True
    (2, 0, 1) -> True
    (1, 0, 2) -> True
    (0, 0, 3) -> True
    _ -> False
    where pairs = filter (==2) $ elems count
          threes = filter (==3) $ elems count
          (count, jokers) = countCards hand

threeOfAKind hand = elem (max 0 $ 3 - jokers) $ elems count
    where (count, jokers) = countCards hand

twoPairs hand = case (length pairs, jokers) of
    (2, _) -> True
    (1, 0) -> False
    (1, _) -> True
    (0, 0) -> False
    (0, 1) -> False
    (0, _) -> True
    where pairs = filter (==2) $ elems count
          (count, jokers) = countCards hand

onePair hand = (jokers >= 1) || elem 2 (elems count)
    where (count, jokers) = countCards hand

highCard _hand = True

emptyHandCount = listArray (Ace, Joker) (repeat 0)
countCards (Hand cards) = (baseCount, length $ filter (== Joker) cards)
    where baseCount = accum (+) emptyHandCount $ map (, 1) $ filter (/=Joker) cards
          withJokers = accum (+) baseCount $ [(card, jokersInHand) | card <- [minBound..maxBound]]
          jokersInHand = length $ filter (== Joker) cards

rules :: [Hand -> Bool]
rules = [fiveOfAKind, fourOfAKind, fullHouse, threeOfAKind, twoPairs, onePair, highCard]

matchRules hand = fromJust $ findIndex (\rule -> rule hand) rules

applyWinnings = zip [1..]
