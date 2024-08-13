#!/bin/bash

set -eu

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

# Building ibus-keyman will also build dependency keyman-system-service
cd "$BASEDIR/ibus-keyman"
./build.sh clean configure

cd "$BASEDIR/keyman-config"
./build.sh clean

cd "$BASEDIR/keyman-config/keyman_config"
export QUILT_PATCHES="${BASEDIR}/debian/patches"
export QUILT_REFRESH_ARGS="-p ab --no-timestamps --no-index"
quilt push -a || true
quilt new version_py.diff
quilt add "version.py"

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
quilt refresh
quilt pop -a
cd ../buildtools && python3 ./build-langtags.py
cd "$BASEDIR"
