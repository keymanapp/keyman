#!/usr/bin/env bash
#
# Extracts and returns the fixture contents of `host-fixture.html`.
#

# This script operates from the perspective of its own directory.
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
cd $(dirname "$THIS_SCRIPT")

FIXTURE=`cat host-fixture.html`

# Drops most whitespace, unfortunately.  But it does the job otherwise, so... we'll take it.
echo $FIXTURE | sed -e "s/.*<body>\(.*\)<\/body>.*/\1/"