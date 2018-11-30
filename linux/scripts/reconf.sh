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
autotool_projects="kmflcomp libkmfl ibus-kmfl"
extra_project="keyman-config"

if [ "$1" != "" ]; then
    if [ ! -d "$1" ]; then
        echo "project $1 does not exist"
        exit 1
    fi
    if [ "$1" == "keyman-config" ]; then
        autotool_projects=""
    else
        autotool_projects="$1"
        extra_project=""
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

if [ "${extra_project}" == "keyman-config" ]; then
    majorvers=`cat ../resources/VERSION.md | cut -d . -f 1`
    cd keyman-config/keyman_config
    sed -e "s/_VERSION_/${newvers}/g" -e "s/_MAJORVERSION_/${majorvers}/g" version.py.in > version.py
fi
cd $BASEDIR

# reset VERSION file
echo "${oldvers}" > VERSION
