#!/bin/bash

if [ -z "$1" ]
then
    echo "Please input extension name"
else
    mkdir -p extensions/$1/main/kotlin/org/hoshino9/luogu/$1
    echo "Finished"
fi