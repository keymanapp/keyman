#!/usr/bin/env bash

#
# Upload Debug Information Files for this developer folder
#
# We only want .pdb and .sym files at this time -- we should not need the
# symtabs that PE files give us as the full debug symbols give us better
# coverage anyway.
#
# This script also uploads sourcemap data for Developer web pages.
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

cd "$KEYMAN_ROOT/developer/src"

#
# Remove some additional unnecessary files: we don't want to upload these
# because they are really only used for internal test or as samples.
#

[ -f ./samples/imsample/IMSample.pdb ] && rm -f ./samples/imsample/IMSample.pdb
[ -f ./kmcmpdll/Win32/Release/kcframe.pdb ] && rm -f ./kmcmpdll/Win32/Release/kcframe.pdb
[ -f ./kmcmpdll/Win32/Debug/kcframe.pdb ] && rm -f ./kmcmpdll/Win32/Debug/kcframe.pdb
[ -f ./kmcmpdll/x64/Release/kcframe.x64.pdb ] && rm -f ./kmcmpdll/x64/Release/kcframe.x64.pdb
[ -f ./kmcmpdll/x64/Debug/kcframe.x64.pdb ] && rm -f ./kmcmpdll/x64/Debug/kcframe.x64.pdb

#
# Upload the files
#

sourcemap_paths=(
  ./TIKE/xml
  ../bin/server
  ./kmc/build
  ./kmc-analyze/build
  ./kmc-keyboard-info/build
  ./kmc-kmn/build
  ./kmc-ldml/build
  ./kmc-model/build
  ./kmc-model-info/build
  ./kmc-package/build
)

echo "Uploading symbols for developer/"
./kmc/node_modules/.bin/sentry-cli upload-dif \
   --project keyman-developer \
   --include-sources \
   --no-zips \
   .
./kmc/node_modules/.bin/sentry-cli sourcemaps upload \
   --org keyman \
   --project keyman-developer \
   --release "$VERSION_GIT_TAG"  \
   --dist "$VERSION_ENVIRONMENT" \
   --ext js --ext mjs --ext ts --ext map \
   "${sourcemap_paths[@]}"
