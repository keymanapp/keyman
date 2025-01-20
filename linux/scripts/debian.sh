#!/bin/bash

# Build source packages suitable to import into the Debian package repos
# (https://salsa.debian.org/input-method-team)

# must be run from linux dir

# parameters: [PROJECT=<project>] [DIST=<dist>] [DEBREVISION=<no>] ./scripts/debian.sh
# PROJECT=<project>   only process this project
# DIST=<dist>         distribution to create packages for (unstable, experimental, etc). If not
#                     specified UNRELEASED will be used.
# DEBREVISION=<no>    the debian revision number. Defaults to 1.

set -e

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. $(dirname "$THIS_SCRIPT")/package-build.inc.sh

checkPrerequisites

BASEDIR=$(pwd)

rm -rf debianpackage
mkdir -p debianpackage

for proj in ${projects}; do
    downloadSource debianpackage

    cd "${proj}-${version}"
    if [[ -n "${DIST}" ]]; then
        EXTRA_ARGS="--distribution ${DIST} --force-distribution"
    fi
    # shellcheck disable=SC2086
    dch --newversion "${version}-${DEBREVISION-1}" ${EXTRA_ARGS} ""
    debuild -d -S -sa -Zxz
    cd "${BASEDIR}"
done
