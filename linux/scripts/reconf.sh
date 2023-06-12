#!/bin/bash

set -e

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

BASEDIR="$KEYMAN_ROOT/linux"
echo "basedir is $BASEDIR"

echo "Found tier ${TIER}, version ${VERSION}"

# We need to configure+build core before we can configure ibus-keyman
cd ../core
./build.sh --no-tests clean:arch configure:arch build:arch

cd "$BASEDIR/ibus-keyman"
./build.sh clean configure

cd "$BASEDIR/keyman-config"
./build.sh clean

cd keyman_config
sed \
    -e "s/_VERSION_/${VERSION}/g" \
    -e "s/_VERSIONWITHTAG_/${VERSION_WITH_TAG}/g" \
    -e "s/_VERSIONGITTAG_/${VERSION_GIT_TAG}/g" \
    -e "s/_MAJORVERSION_/${VERSION_MAJOR}/g" \
    -e "s/_RELEASEVERSION_/${VERSION_RELEASE}/g" \
    -e "s/_TIER_/${TIER}/g" \
    -e "s/_ENVIRONMENT_/${VERSION_ENVIRONMENT}/g" \
    -e "s/_UPLOADSENTRY_/${UPLOAD_SENTRY}/g" \
    version.py.in > version.py
cd ../buildtools && python3 ./build-langtags.py
cd "$BASEDIR"
