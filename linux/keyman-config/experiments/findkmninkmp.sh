#!/bin/bash

echo "cached kmp with kmn files inside them:"
find ~/.cache/keyman/ -print0| while read -d $'\0' file
do
	unzip -q -t "$file" *.kmn 2>&1 
	if [ "$?" == "0" ]; then
		basename "$file" .kmp
#	else
#		echo "no kmn in $file"
	fi
done
