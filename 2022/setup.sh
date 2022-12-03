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

echo "import Data.List.Split" > $DIRNAME.hs
echo "" >> $DIRNAME.hs
echo 'main = process <$> readFile "in" >>= print' >> $DIRNAME.hs
echo '' >> $DIRNAME.hs
echo 'process = undefined' >> $DIRNAME.hs
