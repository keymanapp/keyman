#!/usr/bin/env bash

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

THIS_DIR="$(dirname "$THIS_SCRIPT")"

mkdir -p "$THIS_DIR/host"
cp -R "$KEYMAN_ROOT/android/KMEA/app/src/main/assets/"* "$THIS_DIR/host/"
cp "$KEYMAN_ROOT/web/build/app/embed/debug/keyman.js" "$THIS_DIR/host/keymanandroid.js"
cp "$KEYMAN_ROOT/web/build/app/embed/debug/keyman.js.map" "$THIS_DIR/host/keyman.js.map"
cp "$KEYMAN_ROOT/web/build/app/embed/debug/osk/kmwosk.css" "$THIS_DIR/host/kmwosk.css"
cp "$KEYMAN_ROOT/web/build/app/embed/debug/osk/globe-hint.css" "$THIS_DIR/host/globe-hint.css"
cp "$KEYMAN_ROOT/web/build/app/embed/debug/osk/keymanweb-osk.ttf" "$THIS_DIR/host/keymanweb-osk.ttf"

# We shouldn't rely on a prior Android build for these files!  We can retrieve them separately.
cp "$KEYMAN_ROOT/common/web/sentry-manager/build/index.js" "$THIS_DIR/host/keyman-sentry.js"
cp "$KEYMAN_ROOT/node_modules/es6-shim/es6-shim.min.js" "$THIS_DIR/host/es6-shim.min.js"

# Test keyboards

cp "$KEYMAN_ROOT/web/src/test/manual/web/web_context_tests.js" "$THIS_DIR/host/"
# android\KMEA\app\src\main\assets\keyboard.html