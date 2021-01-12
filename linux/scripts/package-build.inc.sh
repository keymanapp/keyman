#!/bin/bash

function checkPrerequisites() {
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

    if [ ! $(which xmllint) ]; then
        echo "you must install xmllint (libxml2-utils package) to use this script"
        exit 1
    fi

    if [ "${PROJECT}" != "" ]; then
        projects="${PROJECT}"
    else
        projects="keyman-keyboardprocessor kmflcomp libkmfl ibus-kmfl keyman-config ibus-keyman"
    fi
}

function downloadSource() {
    local packageDir
    packageDir=$1

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
    mv ${proj}-${version} ${BASEDIR}/${packageDir}
    mv ${proj}_${version}.orig.tar.gz ${BASEDIR}/${packageDir}
    mv ${proj}-${version}.tar.gz ${BASEDIR}/${packageDir}
    rm ${proj}*.debian.tar.xz
    cd ${BASEDIR}/${packageDir}
    wget -N https://downloads.keyman.com/linux/${TIER}/${dirversion}/SHA256SUMS
    sha256sum -c --ignore-missing SHA256SUMS |grep ${proj}
}