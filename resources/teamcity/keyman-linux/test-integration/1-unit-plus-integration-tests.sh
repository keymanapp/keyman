#!/usr/bin/env bash
# Step name: Unit plus integration tests

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

cd "${REPO_ROOT}/linux" || exit 1

rm -f /tmp/ibus-engine-keyman.log*
rm -f /tmp/ibus-daemon.log
rm -f /tmp/debug.output
# symlink might point to wrong location, so delete it - will be re-created during tests
rm -rf ~/.local/share/keyman/test_kmx

export NO_AT_BRIDGE=1
./build.sh clean configure build
./build.sh test
