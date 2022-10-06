#!/usr/bin/env bash
#
# Build Keyman Engine for Android, Keyman for Android, and FirstVoices Android app

set -eu
# set -x: Debugging use, print each statement
# set -x

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

################################ Main script ################################

builder_describe \
  "Build Keyman Engine for Android, Keyman for Android, and FirstVoices Android app." \
  clean \
  build \
  "publish                Publishes the APKs to the Play Store." \
  ":app                   Keyman for Android" \
  ":engine                Keyman Engine for Android" \
  ":samples               Sample and Test apps" \
  ":fv                    OEM FirstVoices app" \
  "--ci                   Don't start the Gradle daemon. Use for CI" \
  "--debug,-d             Local debug build; use for development builds" \
  "--download-resources   Download asset .kmp files from downloads.keyman.com" \
  "--no-kmw-build,-nkmwb  Don't build KMW. Just copy existing artifacts" \
  "--no-kmw,-nkmw         Don't build KMW. Don't copy artifacts" \
  "--upload-sentry,-us    Uploads debug symbols, etc, to Sentry" 

builder_parse "$@"

KMEA_FLAGS=""
KMAPRO_FLAGS=""
SAMPLE_FLAGS=""
# FV will always use these
FV_FLAGS="-download-resources -lib-nobuild"

# Build flags that only apply to KMEA
if builder_has_option --no-kmw-build; then
  KMEA_FLAGS="-no-kmw-build"
elif builder_has_option --no-kmw; then
  KMEA_FLAGS="-no-kmw"
fi

# Build flags that apply to apps that include asset .kmp files
if builder_has_option --download-resources; then
  KMAPRO_FLAGs="$KMAPRO_FLAGS -download-resources"
fi

# Build flags that apply to all targets
if builder_has_option --ci; then
  KMEA_FLAGS="$KMEA_FLAGS -no-daemon"
  KMAPRO_FLAGS="$KMAPRO_FLAGS -no-daemon"
  SAMPLE_FLAGS="$SAMPLE_FLAGS -no-daemon"
  FV_FLAGS="$FV_FLAGS -no-daemon"
fi

if builder_has_option --debug; then
  KMEA_FLAGS="$KMEA_FLAGS -debug"
  KMAPRO_FLAGS="$KMAPRO_FLAGS -debug"
  SAMPLE_FLAGS="$SAMPLE_FLAGS -debug"
  FV_FLAGS="$FV_FLAGS -debug"
fi

if builder_has_option --upload-sentry; then
  KMEA_FLAGS="$KMEA_FLAGS -upload-sentry"
  KMAPRO_FLAGS="$KMAPRO_FLAGS -upload-sentry"
  FV_FLAGS="$FV_FLAGS -upload-sentry"
fi

#
# Prevents 'clear' on exit of mingw64 bash shell
#
SHLVL=0

# Clean build artifacts: keyman-engine.aar libaries, output and upload directories
function _clean() {
  cd "$KEYMAN_ROOT/android"

  find . -name "keyman-engine.aar" | while read fname; do
    echo "Cleaning $fname"
    rm $fname
  done
  if [ -f "$KEYMAN_ROOT/oem/firstvoices/android/app/libs/keyman-engine.aar" ]; then
    echo "Cleaning OEM FirstVoices keyman-engine.aar"
    rm "$KEYMAN_ROOT/oem/firstvoices/android/app/libs/keyman-engine.aar"
  fi

  if [ -d "$KEYMAN_ROOT/android/KMAPro/kMAPro/build/outputs" ]; then
    echo "Cleaning KMAPro build outputs directory"
    rm -rf "$KEYMAN_ROOT/android/KMAPro/kMAPro/build/outputs"
  fi

  if [ -d "$KEYMAN_ROOT/android/upload" ]; then
    echo "Cleaning upload directory"
    rm -rf "$KEYMAN_ROOT/android/upload"
  fi
}

function _build_engine() {
  cd "$KEYMAN_ROOT/android/KMEA"
  ./build.sh $KMEA_FLAGS

  if [ $? -ne 0 ]; then
    die "ERROR: KMEA/build.sh failed"
  fi
}

function _build_app() {
  cd "$KEYMAN_ROOT/android/KMAPro"
  ./build.sh $KMAPRO_FLAGS

  if [ $? -ne 0 ]; then
    die "ERROR: KMAPro/build.sh failed"
  fi
}

function _build_samples() {
  cd "$KEYMAN_ROOT/android/Samples/KMSample1"
  ./build.sh $SAMPLE_FLAGS

  if [ $? -ne 0 ]; then
    die "ERROR: KMSample1/build.sh failed"
  fi

  cd "$KEYMAN_ROOT/android/Samples/KMSample2"
  ./build.sh SAMPLE_FLAGS

  if [ $? -ne 0 ]; then
    die "ERROR: KMSample2/build.sh failed"
  fi

  cd "$KEYMAN_ROOT/android/Tests/KeyboardHarness"
  ./build.sh $SAMPLE_FLAGS

  if [ $? -ne 0 ]; then
    die "ERROR: KeyboardHarness/build.sh failed"
  fi
}

function _build_fv() {
  pushd "$KEYMAN_ROOT/oem/firstvoices/android"
  ./build.sh $FV_FLAGS

  if [ $? -ne 0 ]; then
    die "ERROR: oem/firstvoices/android/build.sh failed"
  fi
}

# Check about cleaning artifact paths
if builder_start_action clean; then
  _clean
  builder_finish_action success clean
fi

# Building Keyman Engine for Android
if builder_start_action build:engine; then
  _build_engine
  builder_finish_action success build:engine
fi

# Building Keyman for Android
if builder_start_action build:app; then
  _build_app
  builder_finish_action success build:app
fi

# Building Sample apps
if builder_start_action build:samples; then
  _build_samples
  builder_finish_action success build:samples
fi

cd "$KEYMAN_ROOT/android"

# Building OEM apps
if builder_start_action build:fv; then
  _build_fv
  builder_finish_action success build:fv
fi

cd "$KEYMAN_ROOT/android"

# Publish Keyman for Android to Play Store
if builder_start_action publish:app; then
  echo "publishing Keyman for Android"

  $KEYMAN_ROOT/android/build-publish.sh -no-daemon -kmapro
  builder_finish_action success publish:app
fi

if builder_start_action publish:fv; then
  echo "publishing OEM FirstVoices app"

  $KEYMAN_ROOT/android/build-publish.sh -no-daemon -fv
  builder_finish_action success publish:fv
fi
