#!/usr/bin/env bash
#
# Build Keyman Engine for Android, Keyman for Android, OEM FirstVoices Android app,
# Samples: KMsample1 and KMSample2, Test - KeyboardHarness

#set -x
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

################################ Main script ################################

builder_describe \
  "Build Keyman Engine for Android, Keyman for Android, and FirstVoices Android app." \
  clean \
  configure \
  build \
  test \
  "publish                                  Publishes the APKs to the Play Store." \
  ":engine=KMEA                             Keyman Engine for Android" \
  ":app=KMAPro                              Keyman for Android" \
  ":sample1=Samples/KMSample1               Sample app: KMSample1" \
  ":sample2=Samples/KMSample2               Sample app: KMSample2" \
  ":keyboardharness=Tests/KeyboardHarness   Test app: KeyboardHarness" \
  ":fv=../oem/firstvoices/android           OEM FirstVoices for Android app"

builder_parse "$@"

CONFIG=release
DEBUG_FLAG=
CI_FLAG=
SENTRY_FLAG=

if builder_has_option --ci; then
  CI_FLAG=--ci
fi

if builder_has_option --debug; then
  CONFIG=debug
  DEBUG_FLAG=--debug
fi

builder_run_child_actions clean configure build test publish

if builder_has_option --upload-sentry; then
  SENTRY_FLAG="--upload-sentry"
fi

# TODO: 
# builder_declare_inheritable_parameters \
#  "--ci                                     Don't start the Gradle daemon. Use for CI" \
#  "--upload-sentry                          Uploads debug symbols, etc, to Sentry"

# This script also responsible for cleaning up /android/upload
if builder_start_action clean; then

  builder_heading "Cleanup /android/upload"
  rm -rf "$KEYMAN_ROOT/android/upload"
  builder_finish_action success clean
fi
