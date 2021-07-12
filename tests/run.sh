#!/bin/bash

touch temp1
touch temp2
touch temp3
touch temp4

echo $1> temp1
sed 's+.jasm+.class+g' temp1 > temp2
sed 's+.class++g' temp2 > temp3
sed 's+./tests/++g' temp3 > temp4

jsrc=$(cat temp1)
csrc=$(cat temp2)
bsrc=$(cat temp4)

rm temp1
rm temp2
rm temp3
rm temp4

echo $jsrc
echo $csrc
echo $bsrc

java -jar asmtools.jar jasm $jsrc > $csrc
java $bsrc
