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

#for file in `find ~/.cache/keyman -name "*.kmp"`
#do
#	unzip -q -t "$file" *.kmn 2>&1 
#	if [ "$?" == "0" ]; then
#		echo "$file"
##	else
##		echo "no kmn in $file"
#	fi
#done
