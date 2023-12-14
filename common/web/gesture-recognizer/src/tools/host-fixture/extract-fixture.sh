#!/usr/bin/env bash
#
# Extracts and returns the fixture contents of `host-fixture.html`.
#

set -eu

# This script operates from the perspective of its own directory.
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
cd $(dirname "$THIS_SCRIPT")

# On Windows, `node extractor.cjs` cannot be directly piped; that requires `node.exe`.
# But that doesn't make sense on other platforms, so... wrapper script.
node extractor.cjs