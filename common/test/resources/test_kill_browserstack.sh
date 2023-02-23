#! /usr/bin/env bash
#
# CI script to kill BrowserStack testing tunnel (Windows only)
#

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE


if [[ $BUILDER_OS == win ]]; then
  # BrowserStackLocal may not exist, so always pass
  taskkill //f //im BrowserStackLocal.exe || true
fi

exit 0
