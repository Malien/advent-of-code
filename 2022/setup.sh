if [[ $1 -lt 10 ]]
then
  DIRNAME="0$1"
else
  DIRNAME="$1"
fi
mkdir -p $DIRNAME
cd $DIRNAME
echo "source ../.env" > download.sh
echo "curl -H 'Accept:application/json' -H Cookie:session=\$AOC_SESSION https://adventofcode.com/2022/day/$1/input > in" >> download.sh
chmod +x download.sh

cp ../.nvimrc.lua .

echo "import           Data.List" > $DIRNAME.hs
echo "import           Data.List.Split" >> $DIRNAME.hs
echo "import           Data.Ord" >> $DIRNAME.hs
echo "import           Data.Char" >> $DIRNAME.hs
echo "import           Data.Maybe" >> $DIRNAME.hs
echo "import           Debug.Trace" >> $DIRNAME.hs
echo "import           Text.Regex.PCRE" >> $DIRNAME.hs
echo "import           Data.Array" >> $DIRNAME.hs
echo "import           Data.Map        (Map)" >> $DIRNAME.hs
echo "import qualified Data.Map        as Map" >> $DIRNAME.hs
echo "import           Data.Set        (Set)" >> $DIRNAME.hs
echo "import qualified Data.Set        as Set" >> $DIRNAME.hs
echo "" >> $DIRNAME.hs
echo 'main = process <$> readFile "in" >>= print' >> $DIRNAME.hs
echo '' >> $DIRNAME.hs
echo 'test = ""' >> $DIRNAME.hs
echo '' >> $DIRNAME.hs
echo 'process = lines' >> $DIRNAME.hs
