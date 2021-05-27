#!/bin/bash
# Build KMAPro

# Set sensible script defaults:
# set -e: Terminate script if a command returns an error
set -e
# set -u: Terminate script if an unset variable is used
set -u
# set -x: Debugging use, print each statement
# set -x

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/build-download-resources.sh"

echo Build KMAPro

#
# Prevents 'clear' on exit of mingw64 bash shell
#
SHLVL=0

display_usage ( ) {
    echo "build.sh [-no-daemon] [-debug] [-download-resources]"
    echo
    echo "Build Keyman for Android"
    echo "  -no-daemon              Don't start the Gradle daemon. Use for CI"
    echo "  -upload-sentry          Uploads debug symbols, etc, to Sentry"
    echo "  -debug                  Compile only Debug variant"
    echo "  -download-resources     Download sil_euro_latin.kmp and nrc.en.mtnt.model.kmp from downloads.keyman.com"
    exit 1
}

function makeLocalSentryRelease() {
  local SENTRY_RELEASE_VERSION="release-$VERSION_WITH_TAG"
  echo "Making a Sentry release for tag $SENTRY_RELEASE_VERSION"
  sentry-cli upload-dif -p keyman-android --include-sources
  sentry-cli releases -p keyman-android files $SENTRY_RELEASE_VERSION upload-sourcemaps ./

  echo "Finalizing release tag $SENTRY_RELEASE_VERSION"
  sentry-cli releases finalize "$SENTRY_RELEASE_VERSION"
}

NO_DAEMON=false
ONLY_DEBUG=false
DO_KEYBOARDS_DOWNLOAD=false
DO_MODELS_DOWNLOAD=false
DO_SENTRY_LOCAL_UPLOAD=false

# Parse args
while [[ $# -gt 0 ]] ; do
    key="$1"
    case $key in
        -no-daemon)
            NO_DAEMON=true
            ;;
        -upload-sentry)
            # Overrides default set by build-utils.sh
            UPLOAD_SENTRY=true
            ;;
        -debug)
            ONLY_DEBUG=true
            ;;
        -download-resources)
            DO_KEYBOARDS_DOWNLOAD=true
            DO_MODELS_DOWNLOAD=true
            ;;
        -h|-\?)
            display_usage
            ;;
    esac
    shift # past argument
done

KEYBOARD_PACKAGE_ID="sil_euro_latin"
KEYBOARDS_TARGET="$KEYMAN_ROOT/android/KMAPro/kMAPro/src/main/assets/${KEYBOARD_PACKAGE_ID}.kmp"
MODEL_PACKAGE_ID="nrc.en.mtnt"
MODELS_TARGET="$KEYMAN_ROOT/android/KMAPro/kMAPro/src/main/assets/${MODEL_PACKAGE_ID}.model.kmp"

# Verify default keyboard and dictionary exist
if [[ ! -f "$KEYBOARDS_TARGET" ]]; then
  echo "$KEYBOARDS_TARGET doesn't exist. Will download the latest version"
  DO_KEYBOARDS_DOWNLOAD=true
fi

if [[ ! -f "$MODELS_TARGET" ]]; then
  echo "$MODELS_TARGET doesn't exist. Will download the latest version"
  DO_MODELS_DOWNLOAD=true
fi

# Local development optimization to upload local symbols to Sentry
if [[ $VERSION_ENVIRONMENT == "local" && $ONLY_DEBUG == true && $UPLOAD_SENTRY == true ]]; then
  DO_SENTRY_LOCAL_UPLOAD=true
fi

echo
echo "NO_DAEMON: $NO_DAEMON"
echo "ONLY_DEBUG: $ONLY_DEBUG"
echo "DO_KEYBOARDS_DOWNLOAD: $DO_KEYBOARDS_DOWNLOAD"
echo "DO_MODELS_DOWNLOAD: $DO_MODELS_DOWNLOAD"
echo "DO_SENTRY_LOCAL_UPLOAD: $DO_SENTRY_LOCAL_UPLOAD"
echo

if [ "$NO_DAEMON" = true ]; then
  DAEMON_FLAG=--no-daemon
else
  DAEMON_FLAG=
fi

# Convert markdown to html for offline help
echo "Converting markdown to html for offline help"
cd "$KEYMAN_ROOT/android"
./build-help.sh htm
cd "$KEYMAN_ROOT/android/KMAPro"

# Download default keyboard and dictionary
if [ "$DO_KEYBOARDS_DOWNLOAD" = true ]; then
  downloadKeyboardPackage "$KEYBOARD_PACKAGE_ID" "$KEYBOARDS_TARGET"
fi

if [ "$DO_MODELS_DOWNLOAD" = true ]; then
  downloadModelPackage "$MODEL_PACKAGE_ID" "$MODELS_TARGET"
fi

if [ "$ONLY_DEBUG" = true ]; then
  BUILD_FLAGS="assembleDebug lintDebug"
else
  # build = assemble + check; check = test + lint
  BUILD_FLAGS=build
fi

echo "BUILD_FLAGS $BUILD_FLAGS"
./gradlew $DAEMON_FLAG clean $BUILD_FLAGS

if [ "$DO_SENTRY_LOCAL_UPLOAD" = true ]; then
  makeLocalSentryRelease
fi