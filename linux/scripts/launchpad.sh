#!/bin/bash

# Build source packages from nightly builds and upload to PPA

# must be run from linux dir

# parameters: [UPLOAD="yes"] [PROJECT="<project>"] [DIST="<dist>"] [PACKAGEVERSION="<version>"] ./scripts/launchpad.sh
# UPLOAD="yes" do the dput for real
# PROJECT="<project>" only upload this project
# DIST="<dist>" only upload for this distribution
# PACKAGEVERSION="<version>" string to append to the package version. Default to "1~sil1"

set -e

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. $(dirname "$THIS_SCRIPT")/package-build.inc.sh

checkPrerequisites

if [ "${UPLOAD}" == "yes" ]; then
    SIM=""
else
    SIM="-s"
fi

if [ "${TIER}" == "stable" ]; then
    ppa="ppa:keymanapp/keyman"
elif [ "${TIER}" == "beta" ]; then
    ppa="ppa:keymanapp/keyman-beta"
else
    ppa="ppa:keymanapp/keyman-alpha"
fi
echo "ppa: ${ppa}"

if [ "${DIST}" != "" ]; then
    distributions="${DIST}"
else
    distributions="bionic focal jammy"
fi

if [ "${PACKAGEVERSION}" != "" ]; then
    packageversion="${PACKAGEVERSION}"
else
    packageversion="1~sil1"
fi


BASEDIR=$(pwd)

rm -rf launchpad
mkdir -p launchpad

for proj in ${projects}; do
    downloadSource launchpad

    cd ${proj}-${version}
    echo $(pwd)
    cp debian/changelog ../${proj}-changelog
    for dist in ${distributions}; do
        cp ../${proj}-changelog debian/changelog
        dch -v ${version}-${packageversion}~${dist} "source package for PPA"
        dch -D ${dist} -r ""
        debuild -d -S -sa -Zxz
    done
    cd ..
    for dist in ${distributions}; do
        dput ${SIM} ${ppa} ${proj}_${version}-${packageversion}~${dist}_source.changes
    done
    cd ${BASEDIR}
done
