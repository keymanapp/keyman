#! /usr/bin/env bash
#
# CI script to kill BrowserStack testing tunnel (Windows only)
#

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-basic.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE


if builder_is_windows; then
  # BrowserStackLocal may not exist, so always pass
  taskkill //f //im BrowserStackLocal.exe || true
fi

exit 0
