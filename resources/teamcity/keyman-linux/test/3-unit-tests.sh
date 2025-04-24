#!/usr/bin/env bash
# Step name: Unit tests
#
# Expected to be run from Keyman repo root directory

. "resources/teamcity/keyman-linux/includes/env.inc.sh"

rm -f /tmp/ibus-engine-keyman.log
rm -f /tmp/ibus-daemon.log
# symlink might point to wrong location, so delete it - will be re-created during tests
rm -rf ~/.local/share/keyman/test_kmx

export NO_AT_BRIDGE=1
linux/build.sh test --coverage --report --no-integration
