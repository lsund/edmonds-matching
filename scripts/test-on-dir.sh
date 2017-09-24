#!/bin/bash

inpath=$1
outpath=$2

echo -n "" > $outpath

for file in $(ls $inpath)
do
    card=$(./a.out $inpath/$file | head -1 | cut -d' ' -f6)
    fname=$(echo $file | cut -d'.' -f1,2)
    echo $fname $card >> $outpath
done
