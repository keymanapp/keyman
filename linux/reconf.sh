#!/bin/bash

# autoreconf autotool projects

# parameters:
# dev = append current GMT datetime to VERSION

set -e

BASEDIR=`pwd`
autotool_projects="kmflcomp libkmfl ibus-kmfl"

if [ "$1" == "dev" ]; then
    vers=`cat VERSION`
    datevers=`date -u +"%Y%m%d%H%M%S"`
    echo "$vers.${datevers}" > VERSION
fi

# autoreconf the projects
for proj in ${autotool_projects}; do
	cd $proj
	echo "Reconfiguring $proj to version `cat VERSION`"
	autoreconf -if
	cd $BASEDIR
done

if [ "$1" == "dev" ]; then
    echo "${vers}" > VERSION
fi
