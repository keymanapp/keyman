#!/usr/bin/env bash
#
# Extracts and returns the fixture contents of `host-fixture.html`.
#

set -eu

# This script operates from the perspective of its own directory.
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
cd $(dirname "$THIS_SCRIPT")

FIXTURE_START="    <!-- START: recognizer host fixture -->"
FIXTURE_END="    <!-- END: recognizer host fixture -->"

sed -n "/$FIXTURE_START/, /$FIXTURE_END/{ /$FIXTURE_START/! { /$FIXTURE_END/! p } }" host-fixture.html