#!/usr/bin/env bash

#
# Upload Debug Information Files for desktop, engine folders
#
# We only want .pdb and .sym files at this time -- we should not need the
# symtabs that PE files give us as the full debug symbols give us better
# coverage anyway. To avoid uploading too many unnecessary files, we only
# upload those three folders.
#
# This script also uploads sourcemap data for Desktop configuration pages.
#
# Prerequisites: SENTRY_AUTH_TOKEN, SENTRY_URL, SENTRY_ORG variables must
# be configured.
#

set -e
set -u

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

cd "$KEYMAN_ROOT/windows/src"

#
# Upload the files
#

echo "Uploading symbols for desktop/"
sentry-cli upload-dif -p keyman-windows -t breakpad -t pdb desktop --include-sources
sentry-cli releases -p keyman-windows files "$VERSION_GIT_TAG" upload-sourcemaps desktop/kmshell/xml

echo "Uploading symbols for engine/"
sentry-cli upload-dif -p keyman-windows -t breakpad -t pdb engine --include-sources
