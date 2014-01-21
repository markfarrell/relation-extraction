#!/bin/bash

green="\e[0;32m"
revert="\e[0m"

echo "Running test: $1"

time while read -r line ; do
    # Filter blank and commented lines
    if [[ ! $line =~ ^\ *#  && ! $line =~ ^$ ]]; then
        echo "$line"
        echo "$line" | source ./berkeley-lazy-cache | ./tools/pretty-print
    else 
        echo -e "${green}$line${revert}"
    fi 
done < "$1"

echo "Done test."  


