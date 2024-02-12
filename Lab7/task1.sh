#!/bin/bash

rm output.txt 2>> basherrors.txt
rm errors.txt 2>> basherrors.txt
rm basherrors.txt 2>> basherrors.txt
"$1" 2 >> output.txt 2>> errors.txt &
PID="$!"
while true; do
    if !(kill -0 $PID 2>> basherrors.txt); then
        "$1" 2 >> output.txt 2>> errors.txt &
        PID="$!"
        echo CHANGED
    fi
    sleep "$2"m
done
