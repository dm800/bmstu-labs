#!/bin/bash

rm basherrors.txt 2>> basherrors.txt

rec() {
  if [ -d "$1" ]; then
    ls "$1" | while read name; do
      rec "$1/$name"
    done
  else
     COUNT=0
     if [[ "$1" == *.c || "$1" == *.h ]]; then
            while IFS= read -r line; do
                if !([ -z line ]); then
                    COUNT=$((COUNT + 1))
                fi
            done < $1
            echo $COUNT
     fi
  fi
}

val=$(rec "$1")
s=0
for i in $val; do
  s=$((s + i))
done
echo $s