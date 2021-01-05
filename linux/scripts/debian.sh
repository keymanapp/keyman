#!/bin/bash

# Build source packages suitable to import into the Debian package repos
# (https://salsa.debian.org/input-method-team)

# must be run from linux dir

# parameters: [PROJECT="<project>"] ./scripts/debian.sh
# PROJECT="<project>" only upload this project

set -e

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# Check the tier
if [[ -z "${TIER}" ]]; then
    echo "TIER.md or \${TIER} must be set to (alpha, beta, stable) to use this script"
    exit 1
fi

if [ ! $(which xmllint) ]; then
    echo "you must install xmllint (libxml2-utils package) to use this script"
    exit 1
fi

if [ "${PROJECT}" != "" ]; then
    projects="${PROJECT}"
else
    projects="keyman-keyboardprocessor kmflcomp libkmfl ibus-kmfl keyman-config ibus-keyman"
fi

BASEDIR=$(pwd)

rm -rf debianpackage
mkdir -p debianpackage

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
    sed "s/\$tier\|alpha\|beta\|stable/(?:beta|stable)/g" $BASEDIR/scripts/watch.in > debian/watch

    version=$(uscan --report --dehs|xmllint --xpath "//dehs/upstream-version/text()" -)
    dirversion=$(uscan --report --dehs|xmllint --xpath "//dehs/upstream-url/text()" - | cut -d/ -f6)
    echo "${proj} version is ${version}"
    uscan
    cd ..
    mv ${proj}-${version} ${BASEDIR}/debianpackage
    mv ${proj}_${version}.orig.tar.gz ${BASEDIR}/debianpackage
    mv ${proj}-${version}.tar.gz ${BASEDIR}/debianpackage
    rm ${proj}*.debian.tar.xz
    cd ${BASEDIR}/debianpackage
    wget -N https://downloads.keyman.com/linux/${TIER}/${dirversion}/SHA256SUMS
    sha256sum -c --ignore-missing SHA256SUMS |grep ${proj}
    cd ${proj}-${version}
    debuild -d -S -sa -Zxz
    cd ${BASEDIR}
done
