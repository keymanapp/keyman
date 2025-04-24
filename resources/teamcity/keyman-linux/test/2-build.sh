#!/usr/bin/env bash
# Step name: Build
#
# Expected to be run from Keyman repo root directory

. "resources/teamcity/keyman-linux/includes/env.inc.sh"

INSTALLDIR="$(mktemp -d)"
mkdir -p "${INSTALLDIR}/$(python3 -c 'import sys;import os;pythonver="python%%d.%%d" %% (sys.version_info[0], sys.version_info[1]);sitedir = os.path.join("lib", pythonver, "site-packages");print(sitedir)')"
DESTDIR="${INSTALLDIR}" ./linux/build.sh --coverage clean configure build install
