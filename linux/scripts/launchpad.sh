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

if [ "${UPLOAD}" == "yes" ]; then
    SIM=""
else
    SIM="-s"
fi

# Check the tier
if [[ -z "${TIER}" ]]; then
    echo "TIER.md or \${TIER} must be set to (alpha, beta, stable) to use this script"
    exit 1
fi

if [ "${TIER}" == "stable" ]; then
    ppa="ppa:keymanapp/keyman"
elif [ "${TIER}" == "beta" ]; then
    ppa="ppa:keymanapp/keyman-beta"
else
    ppa="ppa:keymanapp/keyman-alpha"
fi
echo "ppa: ${ppa}"

if [ ! $(which xmllint) ]; then
    echo "you must install xmllint (libxml2-utils package) to use this script"
    exit 1
fi

if [ "${PROJECT}" != "" ]; then
    projects="${PROJECT}"
else
    projects="keyman-keyboardprocessor kmflcomp libkmfl ibus-kmfl keyman-config ibus-keyman"
fi

if [ "${DIST}" != "" ]; then
    distributions="${DIST}"
else
    distributions="bionic focal groovy"
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
    if [ "${proj}" == "keyman-keyboardprocessor" ]; then
       cd ${BASEDIR}/../common/core/desktop
    else
       cd ${proj}
    fi

    if [ "${proj}" == "keyman-config" ]; then
        make clean
    fi

    # Update tier in Debian watch files (replacing any previously set tier)
    sed "s/\$tier\|alpha\|beta\|stable/${TIER}/g" $BASEDIR/scripts/watch.in > debian/watch

    version=$(uscan --report --dehs|xmllint --xpath "//dehs/upstream-version/text()" -)
    dirversion=$(uscan --report --dehs|xmllint --xpath "//dehs/upstream-url/text()" - | cut -d/ -f6)
    echo "${proj} version is ${version}"
    uscan
    cd ..
    mv ${proj}-${version} ${BASEDIR}/launchpad
    mv ${proj}_${version}.orig.tar.gz ${BASEDIR}/launchpad
    mv ${proj}-${version}.tar.gz ${BASEDIR}/launchpad
    rm ${proj}*.debian.tar.xz
    cd ${BASEDIR}/launchpad
    wget -N https://downloads.keyman.com/linux/${TIER}/${dirversion}/SHA256SUMS
    sha256sum -c --ignore-missing SHA256SUMS |grep ${proj}
    cd ${proj}-${version}
    echo $(pwd)
    cp debian/changelog ../${proj}-changelog
    #TODO separate source builds and dputs for each of $dists?
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
