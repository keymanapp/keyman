#!/bin/bash

# compares the version of this branch (in ../resources/VERSION.md) to the 
#	version in the Xcode project file of the subproject named in $1
# returns zero if they are the same, non-zero if not the same or a problem occurs

if [ $# -eq 1 ]
then
	echo the argument is $1
	ls -l ./$1/$1.xcodeproj/project.pbxproj || exit 1
else
	echo there must be exactly one argument, e.g. Keyman4MacIM
	exit 1;
fi

# priming PRODUCT_VERSION to -99.9
PRODUCT_VERSION=-99.9

# grepping for PRODUCT_VERSION
PROJ_PROD_VERSION_LINE=`grep "PRODUCT_VERSION = " ./$1/$1.xcodeproj/project.pbxproj | head -1`

# the grepped line is
# $PROJ_PROD_VERSION_LINE

# writing the line to PROJ_VERSION.md with exact spacing
echo $PROJ_PROD_VERSION_LINE | sed /\ /s///g > ./PROJ_VERSION.md

# the exact line is now
# cat PROJ_VERSION.md

# executing the modified line
. ./PROJ_VERSION.md
echo Product version in the project file PRODUCT_VERSION is $PRODUCT_VERSION

KM_VERSION=`cat ../resources/VERSION.md`

echo Keyman version KM_VERSION is $KM_VERSION

if [ x$KM_VERSION = x$PRODUCT_VERSION ]
then
	echo the versions are the same, all is well.
	# clean up
	rm ./PROJ_VERSION.md
	exit 0
else
	echo the version $KM_VERSION and $PRODUCT_VERSION are NOT the same!
	exit 1
fi

