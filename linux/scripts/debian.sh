#!/bin/bash

# Build source packages suitable to import into the Debian package repos
# (https://salsa.debian.org/input-method-team)

# must be run from linux dir

# parameters: [PROJECT="<project>"] ./scripts/debian.sh
# PROJECT="<project>" only process this project

set -e

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. $(dirname "$THIS_SCRIPT")/package-build.inc.sh

checkPrerequisites

BASEDIR=$(pwd)

rm -rf debianpackage
mkdir -p debianpackage

for proj in ${projects}; do
    downloadSource debianpackage

    cd ${proj}-${version}
    dch -v ${version}-1 "Re-release to Debian"
    debuild -d -S -sa -Zxz
    cd ${BASEDIR}
done
