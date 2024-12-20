{-# LANGUAGE QuasiQuotes #-}

import           Data.List
import           Data.List.Split
import           Data.Ord
import           Data.Char
import           Data.Maybe
import           Debug.Trace
import           Text.Regex.PCRE
import           Data.Array
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Text.RawString.QQ (r)

main = readFile "in" >>= print . process

test = tail [r|
r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
|]

singles [] = []
singles ([x]:xs) = x : singles xs
singles (_:xs) = singles xs

isOnlySingles signles = all (`elem` signles)

process input = length $ filter (isDesignPossible $ filterRedundant [] patterns) targets
  where (patterns, targets) = parse input
        -- ss = singles patterns
        -- notRedundant = map (:[]) ss ++ filter (not . isOnlySingles ss) patterns
        -- doubles = filter ((==2) . length) notRedundant
        -- mutated = concat [[s:d, d ++ [s]] | s <- ss, d <- doubles]
        -- notRedundant2 = filter (not . (`elem` mutated)) notRedundant

isRedundant patterns [] = True
isRedundant patterns pattern = any (isRedundant patterns) $ noSuffix ++ noPrefix
  where noSuffix = mapMaybe (`stripSuffix` pattern) patterns
        noPrefix = mapMaybe (`stripPrefix` pattern) patterns

filterRedundant acc (pattern:rest) | isRedundant (acc ++ rest) pattern = filterRedundant acc rest
filterRedundant acc (pattern:rest) = filterRedundant (pattern:acc) rest
filterRedundant acc [] = acc

stripSuffix suffix list | suffix `isSuffixOf` list = Just $ take (length list - length suffix) list
stripSuffix _ _ = Nothing

parse input = (splitOn ", " ps, ts)
  where (ps:_:ts) = lines input

isDesignPossible _ [] = True
isDesignPossible patterns target = any (isDesignPossible patterns) $ mapMaybe (`stripPrefix` target) patterns
