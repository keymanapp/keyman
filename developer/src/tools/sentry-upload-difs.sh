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
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

cd "$KEYMAN_ROOT/developer"

#
# Remove some additional unnecessary files: we don't want to upload these
# because they are really only used for internal test or as samples.
#

excluded_files=(
  src/kmcmplib/build/x64/release/tests/api-test.exe
  src/kmcmplib/build/x64/release/tests/api-test.pdb
  src/kmcmplib/build/x64/release/tests/kmcompxtest.exe
  src/kmcmplib/build/x64/release/tests/kmcompxtest.pdb
  src/kmcmplib/build/x64/release/tests/uset-api-test.exe
  src/kmcmplib/build/x64/release/tests/uset-api-test.pdb
  src/kmcmplib/build/x86/release/tests/api-test.exe
  src/kmcmplib/build/x86/release/tests/api-test.pdb
  src/kmcmplib/build/x86/release/tests/kmcompxtest.exe
  src/kmcmplib/build/x86/release/tests/kmcompxtest.pdb
  src/kmcmplib/build/x86/release/tests/uset-api-test.exe
  src/kmcmplib/build/x86/release/tests/uset-api-test.pdb
  src/samples/imsample/bin/Win32/Release/imsample.dll
  src/samples/imsample/bin/Win32/Release/imsample.pdb
  src/samples/imsample/bin/Win32/Release/imsample.pdb
  src/samples/imsample/bin/x64/Release/imsample.x64.dll
  src/samples/imsample/bin/x64/Release/imsample.x64.pdb
  src/samples/imsample/bin/x64/Release/imsample.x64.pdb
)

rm -f "${excluded_files[@]}"

#
# Upload the files
#

sourcemap_paths=(
  ./src/TIKE/xml
  ./bin/server
  ./src/kmc/build
  ./src/kmc-analyze/build
  ./src/kmc-convert/build
  ./src/kmc-keyboard-info/build
  ./src/kmc-kmn/build
  ./src/kmc-ldml/build
  ./src/kmc-model/build
  ./src/kmc-model-info/build
  ./src/kmc-package/build
)

echo "Uploading symbols for developer/"
sentry-cli upload-dif \
  --project keyman-developer \
  --include-sources \
  --no-zips \
  src/

upload_sourcemap() {
  local smpath="$1"
  sentry-cli sourcemaps upload \
    --no-dedupe \
    --org keyman \
    --project keyman-developer \
    --release "$VERSION_GIT_TAG"  \
    --ext js --ext mjs --ext ts --ext map \
    "$smpath"
}

# Separate sourcemap upload for KeymanWeb to avoid pathing
cd "$KEYMAN_ROOT/developer/bin/server/build/src/site"
upload_sourcemap .
cd "$KEYMAN_ROOT/developer"

# Upload all other sourcemaps
for sourcemap_path in "${sourcemap_paths[@]}"; do
  upload_sourcemap "$sourcemap_path"
done
