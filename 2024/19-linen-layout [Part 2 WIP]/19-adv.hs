{-# LANGUAGE QuasiQuotes #-}

import           Data.Array
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Maybe
import           Data.Ord
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Debug.Trace
import           Text.RawString.QQ (r)
import           Text.Regex.PCRE

main = readFile "in" >>= print . process

test' = tail [r|
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

test = tail [r|
ugw, wbgg, uwu, ww, buuuu, bggu, rrgub, uuwr, rrgr, wwg, bwgw, rw, uwwg, ggrb, rwg, bbug, ggrw, bbb, wruwr, brg, wgrg, grwb, gbg, bggwwrb, rr, bwrw, wbwwb, grw, gbrgww, rrrug, ubuggu, bgb, wbbr, guuwgub, ugwbb, urbw, rbgwg, ugg, bbwu, bggrbuu, gurb, wggrgwb, ugubwg, ru, rwruub, bgw, buug, wgbw, gubwru, wu, uubw, gurw, bbrb, wru, rur, uw, gwbwgr, wwb, wrw, grgugurb, gwuu, rruwwug, www, gur, rgu, ruwgub, rwug, bb, grrw, gbbbwr, brwgg, wrbwgwb, buww, gbug, bgug, rrbgg, bwrrg, rbu, wwuuub, bwwgrr, rgw, grrgru, wuuggg, rwwb, bu, rrr, bgr, brrggrrw, buur, wr, uuuu, rgbrgur, rwwru, bgwuwg, wbbgr, wurubgrg, rbugbu, wwu, ubwwr, wbwbwwgw, wbr, gwbb, rrw, g, gwrg, ugb, ub, ggb, uwrur, gurr, guwwubr, urgg, uwr, bgggru, ugrbg, wbbggb, urur, rubgguu, guwb, rbbwrwbw, bgrb, wrg, wbggb, rbbbgw, uuwuu, urrwg, wg, ruu, brgrwgg, bgbw, wgu, gbw, ruuuugw, wuw, ubu, ggguwb, rwbubbr, rrg, bbrbr, wggbuu, rgg, burwurw, gubrgg, rrb, bbw, urb, urbwrbwb, bugu, rbwug, rgb, ubgbw, wurrrb, bwbbg, rbw, bbubrw, gug, bbg, rruw, uuuwu, bbbr, gbrwrrr, wwgb, wbrrbwg, uwwu, ugwbw, bbwrg, rrbwrb, bgbrggub, gbbbbb, bgbbrgb, rgrg, bubwu, bur, rub, guu, uuw, bru, rbbbbb, urugw, wruw, uwrwur, uug, wgrgwgu, gbgwgu, ggu, grwugu, bubr, urwwg, bbwgurg, urwgr, gwu, ubgwbur, brr, rwu, wub, ggw, bgrurbg, wrwggg, wbg, uwurww, gugug, rubuur, ggwb, wbrw, wubr, rgguu, gruw, bgwug, uwb, uuu, uwgr, wwgu, urw, rubwubu, gugrubw, bwbrg, rgub, gwbrwwg, uu, bgrwwg, gbrggwbr, ugrw, bwg, ugbub, uwbuu, wgbug, wgrbggbw, bgrbr, gwgr, rbub, gwb, gwwu, wwrurwu, uggrww, gub, rrbubgur, uwrr, rbbb, wbgrg, gbwgur, bruru, r, ugrbbr, guwggg, bwugg, wburru, rrrrwu, ugbbw, urwub, gwbu, ubr, uugbwg, ubww, rwuur, ubw, wggbg, bug, grgu, wwgg, wgbuw, ubburgbb, gwugw, ruur, bruu, wbgu, gbgr, rrbr, gggb, wur, guw, grrbrgw, brw, rrrrrwwu, ug, rbgb, guggg, bg, wgg, bugwgbrw, wgbbgru, gwwgbu, gbwurr, wrb, wguuwrg, wwr, uwwwbu, bgu, ugu, ugrwug, wbu, gr, wurgb, rwb, rg, ggbrrbrb, bub, uburg, urbbbb, brrwu, wguwbr, rgbbwuub, urug, buu, bwgb, gru, ggrrwg, gurbr, ruw, bwr, gbr, brb, buuu, brwg, bbrw, gw, wwggr, gg, uru, wbb, rbb, uugb, rgrbu, bwugrb, gb, bgg, wug, ruru, grr, wrr, wubbrww, brguww, uwg, rbr, ubb, bgruu, uwwwrb, grrguuu, wwbbb, wrwgw, rugbu, wubbg, urr, ruwgbbub, grrgr, rrwu, wbwg, gwuwrbr, gburw, brurrguw, ubg, rurbu, bgurugrb, gwbguwb, grrwgubw, burw, rgrr, bwgu, wwurb, wb, urg, rww, gwr, grg, rgr, bbuu, rbg, bubww, uub, ruwbgb, grgbg, ggrwg, uwuuu, bbbbwrb, wuu, ggbg, ubuwru, wugubg, bbr, ggg, bubwg, wgr, b, bgrgw, ruwb, ubuuu, bwu, uggb, ggr, ur, gbu, urwwugb, wrugu, uuurb, rurwr, uguggb, buwg, ugwuw, rgwb, bgrbu, gbww, gbb, uuwu, gwgu, uuguu, bwrwggb, bwb, wrbw, rgrb, ubbwwur, grb, gwrb, bbu, uww, buw, rbubrw, rb, bbbrw, gwg, guugg, gwggbrgw, uuuwb, rug, uguguw, gbbu, u, bww, br, gurwggb, wbrwgr, rurgw, rggb, wgw, wwwbbr, ugbgrr, rwr, brgug, wbwgbr, ugr, rrwg, bbur, ubgwwrr, rurwbbu, wwrg, wugrrb, rguwbrr, guuu, rrurubr

wuuuuuwbwgbuwgbbrwbwgwbgwubwggbugubburuubgrgggwu
|]

isOnlySingles signles = all (`elem` signles)

-- process input = combinations (Set.fromList patterns) . explode [] basis $ "gbrb"
process input = map (length . explode [] basis) targets
  where (patterns, targets) = parse input
        basis = filterRedundant [] patterns
        cc (count, cache) target = (count + traceShowId count', cache')
          where (count', cache') = combinations cache (Set.fromList patterns) target

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

explode :: [String] -> [String] -> String -> [String]
explode acc patterns [] = reverse acc
explode acc patterns target =
  concatMap (\(prefix, tail) -> explode (prefix:acc) patterns tail) $ mapMaybe (`splitPrefix` target) patterns

combinations :: Map [String] Int -> Set String -> [String] -> (Int, Map [String] Int)
combinations cache _ [] = (0, cache)
combinations cache _ [a] = (1, cache)
combinations cache patterns [a, b]
  | (a ++ b) `Set.member` patterns = (2, cache)
  | otherwise = (1, cache)
combinations cache patterns tail@(a:b:rest) = case Map.lookup tail cache of
  Just result -> (result, cache)
  Nothing -> if (a ++ b) `Set.member` patterns
    then (asIfSkipped + asIfCombined, cache'')
    else (asIfSkipped, cache')
  where (asIfSkipped, cache') = combinations cache patterns (b:rest)
        (asIfCombined, cache'') = combinations cache' patterns rest

consecutiveCombinations acc patterns (a:b:rest)
  | (a ++ b) `Set.member` patterns = consecutiveCombinations (a:acc) patterns (b:rest)
  | otherwise = (reverse acc, rest)

groupem acc patterns (a:b:rest)
  | (a ++ b) `Set.member` patterns = groupem (a:acc) patterns (b:rest)
  | otherwise = ([a]:acc, b:rest)
  where () = consecutiveCombinations

-- combinations patterns (a:b:c:rest) = case ((a ++ b) `Set.member` patterns, (b ++ c) `Set.member` patterns) of
--   (False, False) -> combinations patterns (b:c:rest)
--   (False, True) -> combinations patterns (b:c:rest)
--   (True, False) -> 2 * combinations patterns (c:rest)
--   (True, True) -> combinations patterns (c:rest) + combinations patterns (b:c:rest)
-- combinations patterns (a:b:rest)
--   | (a ++ b) `Set.member` patterns = combinations patterns (b:rest) + combinations patterns rest
--   | otherwise = combinations patterns (b:rest)

-- combinations patterns (a:b:rest)
--   | (a ++ b) `Set.member` patterns = asIfSkipped + asIfCombined
--   | otherwise = asIfSkipped
--   where asIfSkipped = combinations patterns (b:rest)
        -- asIfCombined = combinations patterns rest

splitPrefix prefix list | prefix `isPrefixOf` list = Just (splitAt (length prefix) list)
splitPrefix _ _ = Nothing
