#!/bin/bash

#Formats a dmx file to the format required by the algorithm
#usage ./format.sh inputfile outputfile

input=$1
output=$2
temp=temp.txt
echo -n '' > $temp
echo -n '' > $output

echo "$(cat "$input" | head -1 | cut -d' ' -f3,4)" > $temp
echo "$(cat "$input" | sed -n '1!p' | cut -d' ' -f2,3)" >> $temp

i=0
while read p; do
    if [[ $i != 0 ]]; then
        for x in $p
        do
            echo -n $((x - 1))" " >> $output
        done
        echo >> $output
    else
        for x in $p
        do
            echo -n $x" " >> $output
        done
        echo >> $output
    fi
    i=$((i+1))
done <"$temp"

rm $temp
