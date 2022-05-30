#!/bin/bash

#
# Upload Debug Information Files for desktop, engine and developer folders
#
# We only want .pdb and .sym files at this time -- we should not need the
# symtabs that PE files give us as the full debug symbols give us better
# coverage anyway. To avoid uploading too many unnecessary files, we only
# upload those three folders.
#
# This script also uploads sourcemap data for Developer web pages and for
# Desktop configuration pages.
#
# Prerequisites: SENTRY_AUTH_TOKEN, SENTRY_URL, SENTRY_ORG variables must
# be configured.
#

set -e
set -u

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

cd "$KEYMAN_ROOT/windows/src"

#
# Remove some additional unnecessary files: we don't want to upload these
# because they are really only used for internal test or as samples.
#

[ -f ./developer/samples/imsample/IMSample.pdb ] && rm -f ./developer/samples/imsample/IMSample.pdb
[ -f ./developer/kmcmpdll/Win32/Release/kcframe.pdb ] && rm -f ./developer/kmcmpdll/Win32/Release/kcframe.pdb
[ -f ./developer/kmcmpdll/Win32/Debug/kcframe.pdb ] && rm -f ./developer/kmcmpdll/Win32/Debug/kcframe.pdb
[ -f ./developer/kmcmpdll/x64/Release/kcframe.x64.pdb ] && rm -f ./developer/kmcmpdll/x64/Release/kcframe.x64.pdb
[ -f ./developer/kmcmpdll/x64/Debug/kcframe.x64.pdb ] && rm -f ./developer/kmcmpdll/x64/Debug/kcframe.x64.pdb

#
# Upload the files
#

echo "Uploading symbols for desktop/"
sentry-cli upload-dif -p keyman-windows -t breakpad -t pdb desktop --include-sources
sentry-cli releases -p keyman-windows files "release-$VERSION_WITH_TAG" upload-sourcemaps desktop/kmshell/xml

echo "Uploading symbols for engine/"
sentry-cli upload-dif -p keyman-windows -t breakpad -t pdb engine --include-sources

echo "Uploading symbols for developer/"
sentry-cli upload-dif -p keyman-developer -t breakpad -t pdb developer --include-sources
sentry-cli releases -p keyman-developer files "release-$VERSION_WITH_TAG" upload-sourcemaps developer/TIKE/xml
