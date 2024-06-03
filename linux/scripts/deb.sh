#!/bin/bash

# Build Debian source packages of Keyman
# and optionally also binary packages with pbuilder

# It must be run from the keyman/linux directory

# parameters: ./deb.sh [sourcepackage] [dist]
# sourcepackage = only create Debian source package
# dist = only build for this distribution

set -e

all_distributions="focal jammy mantic noble"
distributions=""
echo "all_distributions: ${all_distributions}"

if [ "$1" == "sourcepackage" ]; then
    only_sourcepackage=true
    shift
else
    only_sourcepackage=false
fi

if [ "$1" != "" ]; then
	for dist in ${all_distributions}; do
		if [ "${dist}" == "$1" ]; then
			distributions="$1"
		fi
	done
	if [ "${distributions}" == "" ]; then
		echo "distribution $1 does not exist"
		exit 1
	fi
fi

if [ "${distributions}" == "" ]; then
	distributions="${all_distributions}"
fi

echo "distributions: ${distributions}"

BASEDIR=$(pwd)
#echo "basedir is $BASEDIR"

mkdir -p builddebs

# make the source packages
cd builddebs
vers=$(ls ../dist/keyman_*.orig.tar.gz)
#echo "vers1:${vers}"
vers=${vers##*_}
#echo "vers2:${vers}"
vers=${vers%*.orig.tar.gz}
#echo "vers3:${vers}"
cp -a "../dist/keyman_${vers}.orig.tar.gz" .
tar xfz "keyman_${vers}.orig.tar.gz"
cp -a ../../debian "keyman-${vers}"
cd "keyman-${vers}"
dch -v "${vers}-1" "local build"
echo "keyman-${vers}"
dch --release --distribution unstable ""
debuild -d -S -sa -Zxz -us -uc
cd "$BASEDIR/builddebs"
rm -rf "keyman-${vers}"
cd "$BASEDIR"

if $only_sourcepackage; then
	exit 0
fi

# build the packages with cowbuilder from the source package
cd builddebs
echo "keyman version ${vers}"
rm -rf "keyman-${vers}"
dpkg-source -x "keyman_${vers}-1.dsc"
cd "keyman-${vers}"
for dist in ${distributions}; do
    dch -v "${vers}-1+${dist}" "local build for ${dist}"
    echo "dist: $dist"
    DIST=${dist} pdebuild --pbuilder cowbuilder --buildresult /var/cache/pbuilder/result/"${dist}" -- --basepath /var/cache/pbuilder/base-"${dist}".cow --distribution "${dist}" --override-config --othermirror="deb [trusted=yes] file:/var/cache/pbuilder/result/${dist} ./" --bindmounts /var/cache/pbuilder/result/"${dist}"  --hookdir /var/cache/pbuilder/hook.d/"${dist}"
done
cd "${BASEDIR}"
