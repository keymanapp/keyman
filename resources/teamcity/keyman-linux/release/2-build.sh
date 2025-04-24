#!/usr/bin/env bash
# Step name: Build

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

cd "${REPO_ROOT}/linux" || exit 1

INSTALLDIR="$(mktemp -d)"
mkdir -p "${INSTALLDIR}/$(python3 -c 'import sys;import os;pythonver="python%%d.%%d" %% (sys.version_info[0], sys.version_info[1]);sitedir = os.path.join("lib", pythonver, "site-packages");print(sitedir)')"
DESTDIR="${INSTALLDIR}" ./build.sh clean configure build install
