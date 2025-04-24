#!/usr/bin/env bash
# Step name: Make source tarballs

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

cd "${REPO_ROOT}/linux" || exit 1

rm -rf dist
RELEASE_VERSION="$(cat ../VERSION.md)"
TIER="$(cat ../TIER.md)" JENKINS="yes" make reconf
make tmpsources
mkdir -p "upload/${RELEASE_VERSION}"
cp -a dist/*.tar.gz "upload/${RELEASE_VERSION}"
# shellcheck disable=SC2164
cd "upload/${RELEASE_VERSION}"
sha256sum ./*.tar.gz > SHA256SUMS
