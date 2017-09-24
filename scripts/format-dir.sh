#!/bin/bash

inpath=$1
outpath=$2

mkdir -p $outpath

dir=$1

for file in $(ls $inpath)
do
    ./format-file.sh $inpath/$file $outpath/$file.fmt
done
