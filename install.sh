#!/bin/sh

echo "Fetching additional dependencies..."

mkdir -p ./lib
cd ./lib
wget "https://berkeleyparser.googlecode.com/files/BerkeleyParser-1.7.jar"
wget "https://berkeleyparser.googlecode.com/files/eng_sm6.gr"

cd "../../"

echo "Done"



