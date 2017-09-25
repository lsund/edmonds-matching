#!/bin/zsh

inpath="../data/runtimes/e100-3k/"
resfile="../data/runtimes/plot/data/edges100-3k.txt"

echo -n "" > $resfile
for dir in $(ls $inpath)
do
    cont=$(cat $inpath$dir/res)
    echo -n $dir >> $resfile
    echo $cont >> $resfile
done
