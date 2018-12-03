#!/bin/bash

# autoreconf autotool projects

# parameters: [JENKINS="yes"] ./reconf.sh [proj]
# JENKINS="yes" to set version for jenkins builds
# proj = only reconf this project

set -e

# maybe don't need dev any more
# from git can determine whether to use current tagnum or next
# and whether to use current.datetime
# see ../test.sh for script to bring in
# maybe make it a function to get the minor number?

JENKINS=${JENKINS:="no"}
oldvers=`cat VERSION`

. $(dirname "$0")/version.sh

version

echo "version: ${newvers}"

BASEDIR=`pwd`
autotool_projects="kmflcomp libkmfl ibus-kmfl ibus-keyman"
extra_projects="keyboardprocessor keyman-config"

if [ "$1" != "" ]; then
    if [ "$1" == "keyboardprocessor" ]; then
        extra_projects="keyboardprocessor"
        autotool_projects=""
    elif [ ! -d "$1" ]; then
        echo "project $1 does not exist"
        exit 1
    fi
    if [ "$1" == "keyman-config" ]; then
        extra_projects="keyman-config"
        autotool_projects=""
    else
        autotool_projects="$1"
        extra_projects=""
    fi
fi

echo "${newvers}" > VERSION

# autoreconf the projects
for proj in ${autotool_projects}; do
    if [ "${proj}" != "keyman-config" ]; then
        cd $proj
        echo "Reconfiguring $proj to version `cat VERSION`"
        autoreconf -if
        cd $BASEDIR
    fi
done

for proj in ${extra_projects}; do
    if [ "${proj}" == "keyboardprocessor" ]; then
        rm -rf keyboardprocessor
        meson ../common/engine/keyboardprocessor keyboardprocessor
    fi
    if [ "${proj}" == "keyman-config" ]; then
        majorvers=`cat ../resources/VERSION.md`
        cd keyman-config/keyman_config
        sed -e "s/_VERSION_/${newvers}/g" -e "s/_MAJORVERSION_/${majorvers}/g" version.py.in > version.py
    fi
    cd $BASEDIR
done

# reset VERSION file
echo "${oldvers}" > VERSION
