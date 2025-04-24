#!/usr/bin/env bash
# Step name: Make source tarballs
#
# Expected to be run from Keyman repo root directory

. "resources/teamcity/keyman-linux/includes/env.inc.sh"

cd linux || exit 1

rm -rf dist
make tmpsources
