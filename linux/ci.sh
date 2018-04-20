#!/bin/sh

BASEDIR=`pwd`

for proj in kmflcomp libkmfl ibus-kmfl; do
	mkdir build-$proj
	cd build-$proj
	CPPFLAGS="-I/tmp/kmfl" ../$proj/configure --prefix=/tmp/kmfl && make && make install
	cd $BASEDIR
done
