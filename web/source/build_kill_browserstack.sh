#! /usr/bin/env bash
# 
# CI script to kill BrowserStack testing tunnel (Windows only)
#

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE

get_builder_OS

if [ ${os_id} == 'win' ]; then
  taskkill //f //im BrowserStackLocal.exe
fi
