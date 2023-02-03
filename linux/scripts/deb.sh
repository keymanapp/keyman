#!/bin/bash

# Build Debian source packages of Keyman
# and optionally also binary packages with pbuilder

# It must be run from the keyman/linux directory

# parameters: ./deb.sh [sourcepackage] [proj] [dist]
# sourcepackage = only create Debian source package
# proj = only build for this project
# dist = only build for this distribution

set -e

all_distributions="focal"
distributions=""
all_projects="keyman"
projects=""
echo "all_distributions: ${all_distributions}"
echo "all_projects: ${all_projects}"

if [ "$1" == "sourcepackage" ]; then
	if [ "$2" != "" ]; then
		for proj in ${all_projects}; do
			if [ "${proj}" == "$2" ]; then
				projects="$2"
			fi
		done
		if [ "${projects}" == "" ]; then
			echo "project $2 does not exist"
			exit 1
		fi
	fi
else
	if [ "$1" != "" ]; then
		for proj in ${all_projects}; do
			if [ "${proj}" == "$1" ]; then
				projects="$1"
			fi
		done
		if [ "${projects}" == "" ]; then
			for dist in ${all_distributions}; do
				if [ "${dist}" == "$1" ]; then
					distributions="$1"
					projects="${all_projects}"
				fi
			done
			if [ "${distributions}" == "" ]; then
				echo "project or distribution $1 does not exist"
				exit 1
			fi
		else
			if [ "$2" != "" ]; then
				for dist in ${all_distributions}; do
					if [ "${dist}" == "$2" ]; then
						distributions="$2"
					fi
				done
				if [ "${distributions}" == "" ]; then
					echo "project or distribution $1 does not exist"
					exit 1
				fi
			fi
		fi
	fi
fi

if [ "${projects}" == "" ]; then
	projects="${all_projects}"
fi
if [ "${distributions}" == "" ]; then
	distributions="${all_distributions}"
fi

echo "distributions: ${distributions}"
echo "projects: ${projects}"

BASEDIR=$(pwd)
#echo "basedir is $BASEDIR"

mkdir -p builddebs

# make the source packages
cd builddebs
for proj in ${projects}; do
	vers=$(ls ../dist/${proj}_*.orig.tar.gz)
	#echo "vers1:${vers}"
	vers=${vers##*_}
	#echo "vers2:${vers}"
	vers=${vers%*.orig.tar.gz}
	#echo "vers3:${vers}"
	cp -a "../dist/${proj}_${vers}.orig.tar.gz" .
	tar xfz "${proj}_${vers}.orig.tar.gz"
	if [ "keyman" == "$proj" ]; then
		cp -a ../../debian "${proj}-${vers}"
	fi
	cd "${proj}-${vers}"
	dch -v "${vers}-1" "local build"
	echo "${proj}-${vers}"
	debuild -d -S -sa -Zxz -us -uc
	cd "$BASEDIR/builddebs"
	rm -rf "${proj}-${vers}"
done
cd "$BASEDIR"

if [ "$1" == "sourcepackage" ]; then
	exit 0
fi

# build the packages with cowbuilder from the source package
for proj in ${projects}; do
	cd builddebs
	echo "$proj version ${vers}"
	rm -rf "${proj}-${vers}"
	dpkg-source -x "${proj}_${vers}-1.dsc"
	cd "${proj}-${vers}"
	for dist in ${distributions}; do
		dch -v "${vers}-1+${dist}" "local build for ${dist}"
		echo "dist: $dist"
		DIST=${dist} pdebuild --pbuilder cowbuilder --buildresult /var/cache/pbuilder/result/${dist} -- --basepath /var/cache/pbuilder/base-${dist}.cow --distribution ${dist} --override-config --othermirror="deb [trusted=yes] file:/var/cache/pbuilder/result/${dist} ./" --bindmounts /var/cache/pbuilder/result/${dist}  --hookdir /var/cache/pbuilder/hook.d/${dist}
	done
	cd "${BASEDIR}"
done
