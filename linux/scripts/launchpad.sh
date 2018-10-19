#!/bin/bash

# Build source packages from nightly builds and upload to PPA

# must be run from linux dir

# parameters: [UPLOAD="yes"] [PROJECT="<project>"] [DIST="<dist>"] ./scripts/launchpad.sh
# UPLOAD="yes" do the dput for real
# PROJECT="<project>" only upload this project
# DIST="<dist>" only upload for this distribution


set -e

if [ "${UPLOAD}" == "yes" ]; then
    SIM=""
else
    SIM="-s"
fi

if [ ! `which xmllint` ]; then
    echo "you must install xmllint (libxml2-utils package) to use this script"
    exit 1
fi

if [ "${PROJECT}" != "" ]; then
    projects="${PROJECT}"
else
    projects="kmflcomp libkmfl ibus-kmfl keyman-config"
fi

if [ "${DIST}" != "" ]; then
    distributions="${DIST}"
else
    distributions="xenial bionic cosmic"
fi

BASEDIR=`pwd`

rm -rf launchpad
mkdir -p launchpad

for proj in ${projects}; do
    cd ${proj}
    if [ "${proj}" == "keyman-config" ]; then
        tarname="keyman_config"
        make clean
    else
        tarname="${proj}"
    fi
    version=`uscan --report --dehs|xmllint --xpath "//dehs/upstream-version/text()" -`
    echo "${proj} version is ${version}"
    uscan
    cd ..
    mv ${proj}-${version} launchpad
    mv ${proj}_${version}.orig.tar.gz launchpad
    mv ${tarname}-${version}.tar.gz launchpad
    rm ${proj}*.debian.tar.xz
    cd launchpad
    wget -N https://downloads.keyman.com/linux/alpha/${version}/SHA256SUMS
    sha256sum -c --ignore-missing SHA256SUMS |grep ${tarname}
    cd ${proj}-${version}
    echo `pwd`
    cp debian/changelog ../${proj}-changelog
    #TODO separate source builds and dputs for each of $dists?
    for dist in ${distributions}; do
        cp ../${proj}-changelog debian/changelog
        dch -v ${version}-1~${dist} "source package for PPA"
        dch -D ${dist} -r ""
        debuild -d -S -sa -Zxz
    done
    cd ..
    for dist in ${distributions}; do
        dput ${SIM} ppa:keymanapp/keyman-daily ${proj}_${version}-1~${dist}_source.changes
    done
    cd ${BASEDIR}
done
