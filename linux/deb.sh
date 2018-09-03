#!/bin/bash

# Build Debian packages of Keyman with pbuilder

# It must be run from the keyman/linux directory


set -e

distributions='bionic xenial'
projects='kmflcomp libkmfl ibus-kmfl'
echo "distributions: ${distributions}"

declare -A proj_versions=()
BASEDIR=`pwd`

# autoreconf the projects
for proj in ${projects}; do
	cd $proj
	echo "Reconfiguring $proj"
	autoreconf
	cd $BASEDIR
done

mkdir -p builddebs
mkdir -p builddebs/xenial
mkdir -p builddebs/bionic

for proj in ${projects}; do
	rm -rf deb-$proj
	mkdir deb-$proj
	cd deb-$proj
	../$proj/configure
	make dist
	tmp=`basename *.tar.gz .tar.gz`
	origname=${tmp/$proj-/$proj\_}
	index=`expr index "${origname}" _`
	proj_versions[$proj]=${tmp:$index}
	cp $proj*.tar.gz ../builddebs/${origname}.orig.tar.gz
	cd ${BASEDIR}
done

echo ${proj_versions}


for proj in ${projects}; do
	cd builddebs
	echo "$proj version ${proj_versions[$proj]}"
	rm -rf ${proj}-${proj_versions[$proj]}
	tar xfz ${proj}_${proj_versions[$proj]}.orig.tar.gz
	cp -a ../$proj/debian ${proj}-${proj_versions[$proj]}
	cd ${proj}-${proj_versions[$proj]}
	dch -v ${proj_versions[$proj]}-1${dist} "local build"
	for dist in ${distributions}; do
		echo "$dist"
		pdebuild --pbuilder cowbuilder --buildresult /var/cache/pbuilder/result/${dist} -- --basepath /var/cache/pbuilder/base-${dist}.cow --distribution ${dist} --override-config --othermirror="deb [trusted=yes] file:/var/cache/pbuilder/result/${dist} ./" --bindmounts /var/cache/pbuilder/result/${dist}  --hookdir /var/cache/pbuilder/hook.d/${dist}
	done
	cd ${BASEDIR}
done
