#!/bin/bash

# Build source packages from nightly builds and upload to PPA

# must be run from linux dir

# parameters: [LAUNCHPAD_ID="<me>"] [PROJECT="<project>"] ./scripts/launchpad.sh
# LAUNCHPAD_ID="<me>" to set the launchpad id to dput to
# PROJECT="<project>" only upload this project


set -e

if [ "${LAUNCHPAD_ID}" == "" ]; then
    echo "you must set your LAUNCHPAD_ID in the enviroment for dputting"
    exit 1
fi

if [ ! `which xmllint` ]; then
    echo "you must install xmllint (libxml2-utils package) to use this script"
    exit 1
fi

if [ "${PROJECT}" != "" ]; then
    projects="$PROJECT"
else
    projects="kmflcomp libkmfl ibus-kmfl keyman-config"
fi


BASEDIR=`pwd`
dists="xenial bionic cosmic"

rm -rf launchpad
mkdir -p launchpad

for proj in ${projects}; do
    cd ${proj}
    version=`uscan --report --dehs|xmllint --xpath "//dehs/upstream-version/text()" -`
    echo "${proj} version is ${version}"
    uscan
    mv ../${proj}-${version} ../launchpad
    mv ../${proj}_${version}.orig.tar.gz ../launchpad
    if [ "${proj}" == "keyman-config" ]; then
        mv ../keyman_config-${version}.tar.gz ../launchpad
    else
        mv ../${proj}-${version}.tar.gz ../launchpad
    fi
    rm ../${proj}*.debian.tar.xz
    cd ../launchpad/${proj}-${version}
    echo `pwd`
    dch -v ${version}-1 "source package for PPA"
    #TODO separate source builds and dputs for each of $dists?
    dch -r ""
    debuild -d -S -sa -Zxz -us -uc
    cd ..
    dput -s -u ppa:${LAUNCHPAD_ID}/keyman-daily ${proj}_${version}-1_source.changes
    cd ${BASEDIR}
done