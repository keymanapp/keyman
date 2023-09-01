#!/usr/bin/env bash
# Helper script for 16.0 to generate version.py without redefining
# version variables that we already have in build-utils.sh

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

cd "$THIS_SCRIPT_PATH"

pushd keyman_config
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
popd
