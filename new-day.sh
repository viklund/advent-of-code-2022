#!/usr/bin/env bash

day=$(date +%d)
day_unpadded=$(echo $day | sed 's/^0//')


echo "<$day> <$day_unpadded>"

FILE=${day}.rkt

if [ ! -f $FILE ]; then
    cp TEMPLATE.rkt $FILE
fi

curl \
    -H "Cookie: $(cat cookie)" \
    "https://adventofcode.com/2022/day/${day_unpadded}/input" \
    > ${day}.input

