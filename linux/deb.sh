#!/bin/bash

# Build Debian packages of Keyman with pbuilder

# It must be run from the keyman/linux directory

set -e

distributions="bionic xenial"
autotool_projects="kmflcomp libkmfl ibus-kmfl"
all_projects="${autotool_projects} keyman-config"
echo "distributions: ${distributions}"
echo "autotool_projects: ${autotool_projects}"
echo "all_projects: ${all_projects}"

BASEDIR=`pwd`

mkdir -p builddebs

vers=`ls dist/kmflcomp_*.orig.tar.gz`
echo "${vers}"
vers=${vers##*_}
echo "${vers}"
vers=${vers%*.orig.tar.gz}
echo "${vers}"

cd builddebs
for proj in ${all_projects}; do
	cp -a ../dist/${proj}_${vers}.orig.tar.gz .
	tar xfz ${proj}_${vers}.orig.tar.gz
	cp -a ../${proj}/debian ${proj}-${vers}
	cd ${proj}-${vers}
	dch -v ${vers}-1 "local build"
	echo "${proj}-${vers}"
	debuild -d -S -sa -Zxz -us -uc
	cd $BASEDIR/builddebs
	rm -rf ${proj}-${vers}
done
cd $BASEDIR

if [ "$1" == "sourcepackage" ]; then
	exit 0
fi

for proj in ${all_projects}; do
	cd builddebs
	# echo "$proj version ${vers}"
	# rm -rf ${proj}-${vers}
	# tar xfz ${proj}_${vers}.orig.tar.gz
	# cp -a ../$proj/debian ${proj}-${vers}
	# cd ${proj}-${vers}
	for dist in ${distributions}; do
		dch -v ${vers}-1${dist} "local build"
		echo "$dist"
		pdebuild --pbuilder cowbuilder --buildresult /var/cache/pbuilder/result/${dist} -- --basepath /var/cache/pbuilder/base-${dist}.cow --distribution ${dist} --override-config --othermirror="deb [trusted=yes] file:/var/cache/pbuilder/result/${dist} ./" --bindmounts /var/cache/pbuilder/result/${dist}  --hookdir /var/cache/pbuilder/hook.d/${dist}
	done
	cd ${BASEDIR}
done
