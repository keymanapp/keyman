#!/usr/bin/env bash
#
# Build Keyman Engine for Android, Keyman for Android, OEM FirstVoices Android app,
# Samples: KMsample1 and KMSample2, Test - KeyboardHarness
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

################################ Main script ################################

builder_describe \
  "Build Keyman Engine for Android, Keyman for Android, and FirstVoices Android app." \
  "@/resources/tools/check-markdown  test:help" \
  clean \
  configure \
  build \
  test \
  "publish                                  Publishes symbols to Sentry and the APKs to the Play Store." \
  --ci+ \
  --upload-sentry+ \
  ":engine=KMEA                             Keyman Engine for Android" \
  ":app=KMAPro                              Keyman for Android" \
  ":help                                    Online documentation" \
  ":sample1=Samples/KMSample1               Sample app: KMSample1" \
  ":sample2=Samples/KMSample2               Sample app: KMSample2" \
  ":keyboardharness=Tests/KeyboardHarness   Test app: KeyboardHarness" \
  ":fv=../oem/firstvoices/android           OEM FirstVoices for Android app"

builder_parse "$@"

# Override JAVA_HOME to OpenJDK 11
set_java_home

# This script also responsible for cleaning up /android/upload
builder_run_child_actions clean

if builder_start_action clean; then
  builder_heading "Cleanup /android/upload"
  rm -rf "$KEYMAN_ROOT/android/upload"
  builder_finish_action success clean
fi

builder_run_child_actions configure build test
builder_run_action        test:help    check-markdown  "$KEYMAN_ROOT/android/docs/help"

builder_run_child_actions publish