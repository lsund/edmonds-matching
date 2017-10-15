
baseindir="../data/graphs/random-graphs/haskell/e100-3k/"
baseoutdir="../data/runtimes/e100-3k/"

for dir in $(ls $baseindir)
do
    echo $dir
    inputdir=$baseindir$dir
    outputdir=$baseoutdir$dir
    ./run-algorithm.sh $inputdir $outputdir
done
