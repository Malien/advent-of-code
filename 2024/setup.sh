set -e

if [[ $# -eq 0 ]]
then
  DAY=$(date +%-d)
else
  DAY=$1
fi

if [[ $DAY -lt 10 ]]
then
  DIRNAME="0$DAY"
  echo "Creating day $DAY in $DIRNAME"
else
  DIRNAME="$DAY"
fi

mkdir -p $DIRNAME
cd $DIRNAME
echo "source ../.env" > download.sh
echo "curl -H 'Accept:application/json' -H Cookie:session=\$AOC_SESSION https://adventofcode.com/2024/day/$DAY/input > in" >> download.sh
chmod +x download.sh

cp ../.nvimrc.lua .

echo "\
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

main = readFile \"in\" >>= print . process

test = tail [r|
|]

process = lines
" > $DIRNAME.hs
