#!/bin/bash

# autoreconf autotool projects

# parameters: ./reconf.sh [dev] [proj]
# dev = append current GMT datetime to VERSION
# proj = only reconf this project

set -e

BASEDIR=`pwd`
autotool_projects="kmflcomp libkmfl ibus-kmfl"

if [ "$2" != "" ]; then
    autotool_projects="$2"
    if [ ! -d "$2" ]; then
        echo "project $2 does not exist"
        exit 1
    fi
fi

if [ "$1" == "dev" ]; then
    vers=`cat VERSION`
    datevers=`date -u +"%Y%m%d%H%M%S"`
    echo "$vers.${datevers}" > VERSION
else
    if [ "$1" != "" ]; then
        autotool_projects="$1"
        if [ ! -d "$1" ]; then
            echo "project $1 does not exist"
            exit 1
        fi
    fi
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

if [ "$1" == "dev" ]; then
    echo "${vers}" > VERSION
fi
