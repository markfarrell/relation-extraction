#!/bin/sh

echo "Fetching additional dependencies..."

mkdir -p ./lib
cd ./lib
wget "https://berkeleyparser.googlecode.com/files/BerkeleyParser-1.7.jar"
wget "https://berkeleyparser.googlecode.com/files/eng_sm6.gr"
wget "http://www.slf4j.org/dist/slf4j-1.7.5.tar.gz"
tar xvfz "./slf4j-1.7.5.tar.gz"
rm -f "./slf4j-1.7.5.tar.gz"
cd "./slf4j-1.7.5"
rm -f "./slf4j-log4j12-1.7.5.jar"
rm -f "./slf4j-jcl-1.7.5.jar"
rm -f "./slf4j-nop-1.7.5.jar"
rm -f "./slf4j-simple-1.7.5.jar"

cd "../../"

echo "Done"



