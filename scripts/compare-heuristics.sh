#!/bin/zsh

command="../.stack-work/install/x86_64-linux-tinfo6-nopie/lts-8.23/8.0.2/bin/edmonds-matching"

inputdir=$1
outputFile=$2

echo "EC GM MAX" > $outputFile
for file in $(ls $inputdir)
do
    echo -n "$($command only-ec $inputdir/$file) " >> $outputFile
    echo -n "$($command only-gm $inputdir/$file) " >> $outputFile
    echo    "$($command no $inputdir/$file)"      >> $outputFile
done
