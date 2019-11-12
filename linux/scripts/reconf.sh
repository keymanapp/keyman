#!/bin/bash

# autoreconf autotool projects

# parameters: [JENKINS="yes"] ./reconf.sh [proj]
# JENKINS="yes" to set version for jenkins builds
# proj = only reconf this project

set -e

BASEDIR=`pwd`
echo "basedir is $BASEDIR"
autotool_projects="kmflcomp libkmfl ibus-kmfl ibus-keyman"
extra_projects="keyboardprocessor keyman-config"

if [ "$1" != "" ]; then
    if [ "$1" == "keyboardprocessor" ]; then
    echo "reconfiguring only keyboardprocessor"
        extra_projects="keyboardprocessor"
        autotool_projects=""
    elif [ ! -d "$1" ]; then
        echo "project $1 does not exist"
        exit 1
    elif [ "$1" == "keyman-config" ]; then
    echo "reconfiguring only keyman-config"
        extra_projects="keyman-config"
        autotool_projects=""
    else
    echo "reconfiguring only $1"
        autotool_projects="$1"
        extra_projects=""
    fi
fi

if [ -n $SKIPVERSION ]; then
    oldvers=$(cat OLDVERSION)
    newvers=$(cat VERSION)
else
    JENKINS=${JENKINS:="no"}
    oldvers=`cat VERSION`

    . $(dirname "$0")/version.sh

    version

    echo "version: ${newvers}"
    echo "${newvers}" > VERSION
fi

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
        sed -i "s/version: '.*'/version: '${newvers}'/" ../common/engine/keyboardprocessor/meson.build
        meson ../common/engine/keyboardprocessor keyboardprocessor
    fi
    if [ "${proj}" == "keyman-config" ]; then
        cd keyman-config
        make clean
        cd keyman_config
        sed -e "s/_VERSION_/${newvers}/g" -e "s/_MAJORVERSION_/${oldvers}/g" version.py.in > version.py
    fi
    cd $BASEDIR
done

# reset VERSION file
echo "${oldvers}" > VERSION
