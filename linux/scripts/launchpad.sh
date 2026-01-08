#!/bin/bash

# Build source packages from nightly builds and upload to PPA

# parameters: [UPLOAD="yes"] [PROJECT="<project>"] [DIST="<dist>"] [PACKAGEVERSION="<version>"] ./scripts/launchpad.sh
# UPLOAD="yes" do the dput for real
# PROJECT="<project>" only upload this project
# DIST="<dist>" only upload for this distribution
# PACKAGEVERSION="<version>" string to append to the package version. Default to "1~sil1"

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
# shellcheck source=resources/build/builder-basic.inc.sh
. "${THIS_SCRIPT%/*}/../../resources/build/builder-basic.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck source=linux/scripts/package-build.inc.sh
. "$(dirname "${THIS_SCRIPT}")/package-build.inc.sh"

cd "${KEYMAN_ROOT}/linux"

checkPrerequisites

if [[ "${KEYMAN_TIER}" == "stable" ]]; then
  ppa="ppa:keymanapp/keyman"
elif [[ "${KEYMAN_TIER}" == "beta" ]]; then
  ppa="ppa:keymanapp/keyman-beta"
else
  ppa="ppa:keymanapp/keyman-alpha"
fi
echo "ppa: ${ppa}"

distributions="${DIST:-jammy noble plucky questing resolute}"
packageversion="${PACKAGEVERSION:-1~sil1}"

BASEDIR=$(pwd)

rm -rf launchpad
mkdir -p launchpad

for proj in ${projects:-}; do
  downloadSource launchpad

  cd "${proj}-${version:-}"
  pwd
  cp debian/changelog "../${proj}-changelog"
  for dist in ${distributions}; do
    cp "../${proj}-changelog" debian/changelog
    dch -v "${version}-${packageversion}~${dist}" "source package for PPA"
    dch -D "${dist}" -r ""
    debuild -d -S -sa -Zxz
  done
  cd ..
  for dist in ${distributions}; do
    # shellcheck disable=SC2086,SC2248  # no quotes for $SIM - it might not be set
    dput ${SIM:-} "${ppa}" "${proj}_${version}-${packageversion}~${dist}_source.changes"
  done
  cd "${BASEDIR}"
done
