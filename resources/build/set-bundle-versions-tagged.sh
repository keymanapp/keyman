#!/usr/bin/env bash

# Calls script in xcode-utils to update the version
# true:  applies VERSION_WITH_TAG to custom KeymanVersionWithTag plist member used for in-app display

source "$KEYMAN_ROOT/resources/build/xcode-utils.sh"
phaseSetBundleVersions true