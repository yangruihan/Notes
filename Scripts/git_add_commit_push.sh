#!/bin/sh

if [ ! -n "$1" ]
then
    echo "Error: You should write the commit info!"
else
    git add .
    git commit -m "\"$1\""
    git push
fi