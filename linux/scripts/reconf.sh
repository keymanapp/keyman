#!/bin/bash

# autoreconf autotool projects

# parameters: [JENKINS="yes"] ./reconf.sh [proj]
# JENKINS="yes" to set version for jenkins builds
# proj = only reconf this project

set -e

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

BASEDIR="$KEYMAN_ROOT/linux"
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

echo "Found tier ${TIER}, version ${VERSION}"

# autoreconf the projects
for proj in ${autotool_projects}; do
    if [ "${proj}" != "keyman-config" ]; then
        cd $proj
        echo "Reconfiguring $proj to version ${VERSION}"
        autoreconf -if
        cd $BASEDIR
    fi
done

for proj in ${extra_projects}; do
    if [ "${proj}" == "keyboardprocessor" ]; then
        rm -rf keyboardprocessor
        cp ../VERSION.md ../common/core/desktop/
        ../common/core/desktop/build.sh -t keyboardprocessor configure
    fi
    if [ "${proj}" == "keyman-config" ]; then
        cd keyman-config
        make clean
        cd keyman_config
        sed \
            -e "s/_VERSION_/${VERSION}/g" \
            -e "s/_VERSIONWITHTAG_/${VERSION_WITH_TAG}/g" \
            -e "s/_MAJORVERSION_/${VERSION_MAJOR}/g" \
            -e "s/_RELEASEVERSION_/${VERSION_RELEASE}/g" \
            -e "s/_TIER_/${TIER}/g" \
            -e "s/_ENVIRONMENT_/${VERSION_ENVIRONMENT}/g" \
            version.py.in > version.py

        cd ../buildtools && python3 ./build-langtags.py
    fi
    cd $BASEDIR
done
