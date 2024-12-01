if [[ $1 -lt 10 ]]
then
  DIRNAME="0$1"
else
  DIRNAME="$1"
fi
mkdir -p $DIRNAME
cd $DIRNAME
echo "source ../.env" > download.sh
echo "curl -H 'Accept:application/json' -H Cookie:session=\$AOC_SESSION https://adventofcode.com/2024/day/$1/input > in" >> download.sh
chmod +x download.sh

cp ../.nvimrc.lua .

echo "\
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

main = readFile \"in\" >>= print . process

test = \"\"

process = lines
" > $DIRNAME.hs
