#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.

# Calls script in xcode-utils to update the version
# true:  applies KEYMAN_VERSION_WITH_TAG to custom KeymanVersionWithTag plist member used for in-app display

source "$KEYMAN_ROOT/resources/build/mac/xcode-utils.inc.sh"
phaseSetBundleVersions true
