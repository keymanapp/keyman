#!/bin/bash
#
# postbuild script run by npm during/after a build
#

# Exit on command failure and when using unset variables:
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE

LOCAL_ROOT="$(dirname "$THIS_SCRIPT")"

mkdir -p "$LOCAL_ROOT/dist/site/"
mkdir -p "$LOCAL_ROOT/dist/win32/"
cp -r "$LOCAL_ROOT/src/site/"** "$LOCAL_ROOT/dist/site/"
cp -r "$LOCAL_ROOT/src/win32/"** "$LOCAL_ROOT/dist/win32/"

replaceVersionStrings "$LOCAL_ROOT/dist/site/lib/sentry/init.js.in" "$LOCAL_ROOT/dist/site/lib/sentry/init.js"
rm "$LOCAL_ROOT/dist/site/lib/sentry/init.js.in"
