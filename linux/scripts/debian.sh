#!/usr/bin/env bash
# Build source packages suitable to import into the Debian package repos
# (https://salsa.debian.org/input-method-team)

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
# shellcheck source=resources/build/builder-full.inc.sh
. "${THIS_SCRIPT%/*}/../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck source=linux/scripts/package-build.inc.sh
. "$(dirname "${THIS_SCRIPT}")/package-build.inc.sh"

builder_describe \
  "Build source packages suitable to import into the Debian package repos" \
  build \
  "--dist=DIST                Distribution to create packages for (unstable, experimental, etc). Default: UNRELEASED." \
  "--debrevision=DEBREVISION  The debian revision number. Default: 1."

builder_parse "$@"

cd "${KEYMAN_ROOT}/linux"

checkPrerequisites

rm -rf debianpackage
mkdir -p debianpackage

downloadSource debianpackage

cd "keyman-${version}"
if builder_has_option --dist; then
    EXTRA_ARGS="--distribution ${DIST} --force-distribution"
fi
# shellcheck disable=SC2086
dch --newversion "${version}-${DEBREVISION:-1}" ${EXTRA_ARGS:-} ""
debuild -d -S -sa -Zxz
