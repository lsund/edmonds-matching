#!/bin/zsh

command="../.stack-work/install/x86_64-linux-tinfo6-nopie/lts-8.23/8.0.2/bin/edmonds-matching"

inputdir=$1
outputdir=$2
no_file=$2"/no"
ec_file=$2"/ec"
gm_file=$2"/gm"

for file in $(ls $inputdir)
do
    mkdir -p $outputdir
    echo $inputdir
    (time ( { $command "no" $inputdir/$file; ls } 2>&3 ) ) 3>&2 2>>$no_file
done

for file in $(ls $inputdir)
do
    echo $inputdir
    mkdir -p $outputdir
    (time ( { $command "ec" $inputdir/$file; ls } 2>&3 ) ) 3>&2 2>>$ec_file
done

for file in $(ls $inputdir)
do
    echo $inputdir
    mkdir -p $outputdir
    (time ( { $command "gm" $inputdir/$file; ls } 2>&3 ) ) 3>&2 2>>$gm_file
done
