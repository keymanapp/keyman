#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Builds a debug-host page simulating Keyman Android's WebView setup for KMW use" \
  "@/common/web/keyman-version" \
  "@/web/src/app/webview" \
  "@/web/src/engine/sentry-manager" \
  clean configure build

builder_describe_outputs \
  configure          /node_modules \
  build              /web/src/test/manual/embed/android-harness/host/keyboard.html

builder_parse "$@"

### CLEAN ACTIONS

if builder_start_action clean; then
  rm -rf build/
  builder_finish_action success clean
fi

### CONFIGURE ACTIONS

if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

if builder_start_action build; then
  mkdir -p "host/osk"
  cp -R "$KEYMAN_ROOT/android/KMEA/app/src/main/assets/"* "host/"
  cp "$KEYMAN_ROOT/web/build/app/webview/debug/keymanweb-webview.js" "host/keymanweb-webview.js"
  cp "$KEYMAN_ROOT/web/build/app/webview/debug/keymanweb-webview.js.map" "host/keymanweb-webview.js.map"
  cp "$KEYMAN_ROOT/web/build/app/resources/osk/ajax-loader.gif" "host/ajax-loader.gif"
  cp "$KEYMAN_ROOT/web/build/app/resources/osk/kmwosk.css" "host/kmwosk.css"
  cp "$KEYMAN_ROOT/web/build/app/resources/osk/globe-hint.css" "host/globe-hint.css"
  cp "$KEYMAN_ROOT/web/build/app/resources/osk/keymanweb-osk.ttf" "host/keymanweb-osk.ttf"

  # We shouldn't rely on a prior Android build for these files!  We can retrieve them separately.
  cp "$KEYMAN_ROOT/web/src/engine/sentry-manager/build/lib/index.js" "host/keyman-sentry.js"
  cp "$KEYMAN_ROOT/node_modules/es6-shim/es6-shim.min.js" "host/es6-shim.min.js"
  cp "$KEYMAN_ROOT/node_modules/@sentry/browser/build/bundle.min.js" "host/sentry.min.js"

  # Test keyboards

  cp "$KEYMAN_ROOT/web/src/test/manual/web/web_context_tests.js" "host/"
  # android\KMEA\app\src\main\assets\keyboard.html
  builder_finish_action success build
fi