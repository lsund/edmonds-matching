#!/bin/zsh

command="../.stack-work/install/x86_64-linux-tinfo6-nopie/lts-8.23/8.0.2/bin/edmonds-matching"

dir="../data/graphs/batch1/"
mkdir -p "../data/runtimes/batch1"
no_file="../data/runtimes/batch1/no"
ec_file="../data/runtimes/batch1/ec"
gm_file="../data/runtimes/batch1/gm"

for file in $(ls $dir)
do
    (time ( { $command "no" $dir$file; ls } 2>&3 ) ) 3>&2 2>>$no_file
done

for file in $(ls $dir)
do
    (time ( { $command "ec" $dir$file; ls } 2>&3 ) ) 3>&2 2>>$ec_file
done

for file in $(ls $dir)
do
    (time ( { $command "gm" $dir$file; ls } 2>&3 ) ) 3>&2 2>>$gm_file
done
