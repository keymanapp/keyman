#!/bin/bash

# autoreconf autotool projects

# parameters: [BUILD_LEGACY=1] ./reconf.sh [proj]
# BUILD_LEGACY=1 to also build legacy KMFL projects
# proj = only reconf this project

set -e

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

BASEDIR="$KEYMAN_ROOT/linux"
echo "basedir is $BASEDIR"
legacy_projects=""
if [ -n "$BUILD_LEGACY" ]; then
    legacy_projects="kmflcomp libkmfl ibus-kmfl"
fi
autotool_projects="ibus-keyman"
extra_projects="keyboardprocessor keyman-config"

if [ "$1" != "" ]; then
    if [ "$1" == "keyman" ]; then
        echo "reconfiguring only keyman"
        extra_projects="keyman"
        legacy_projects=""
        autotool_projects=""
    elif [ "$1" == "keyboardprocessor" ]; then
        echo "reconfiguring only keyboardprocessor"
        extra_projects="keyboardprocessor"
        legacy_projects=""
        autotool_projects=""
    elif [ "$1" == "keyman-config" ]; then
        echo "reconfiguring only keyman-config"
        extra_projects="keyman-config"
        legacy_projects=""
        autotool_projects=""
    elif [ -d "$1" ]; then
        echo "reconfiguring only $1"
        extra_projects=""
        legacy_projects=""
        autotool_projects="$1"
    elif [ -d "legacy/$1" ]; then
        extra_projects=""
        legacy_projects="$1"
        autotool_projects=""
    else
        echo "project $1 does not exist"
        exit 1
    fi
fi

echo "Found tier ${TIER}, version ${VERSION}"

# autoreconf the projects
for proj in ${autotool_projects}; do
    cd $proj
    echo "Reconfiguring $proj to version ${VERSION}"
    autoreconf -if
    rm -rf autom4te.cache
    cd $BASEDIR
done

for proj in ${legacy_projects}; do
    cd legacy/$proj
    echo "Reconfiguring $proj to version ${VERSION}"
    autoreconf -if
    rm -rf autom4te.cache
    cd $BASEDIR
done

for proj in ${extra_projects}; do
    if [ "${proj}" == "keyboardprocessor" -o "${proj}" == "keyman" ]; then
        rm -rf keyboardprocessor
        cp ../VERSION.md ../core/
        ../core/build.sh -t keyboardprocessor configure
    fi
    if [ "${proj}" == "keyman-config"  -o "${proj}" == "keyman" ]; then
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
            -e "s/_UPLOADSENTRY_/${UPLOAD_SENTRY}/g" \
            version.py.in > version.py
        cd ../buildtools && python3 ./build-langtags.py
    fi
    cd $BASEDIR
done
