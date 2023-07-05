#!/usr/bin/env bash
#
# Provides a platform-independent implementation of the build steps used for CI when
# building the Keyman Engine for Web.
#

# set -x
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

WORKING_DIRECTORY=`pwd`

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

# ################################ Main script ################################

S_KEYMAN_COM=

builder_describe "Defines and implements the CI build steps for Keyman Engine for Web (KMW)." \
  "@/web/src/tools/building/sourcemap-root prepare:s.keyman.com" \
  "build" \
  "test                         Runs all unit tests."  \
  "post-test                    Runs post-test cleanup.  Should be run even if a prior step fails." \
  "validate-size                Runs the build-size comparison check" \
  "prepare                      Prepare upload artifacts for specified target(s)" \
  ":s.keyman.com                Target:  builds artifacts for s.keyman.com " \
  ":downloads.keyman.com        Target:  builds artifacts for downloads.keyman.com" \
  "--s.keyman.com=S_KEYMAN_COM  Sets the root location of a checked-out s.keyman.com repo"

builder_parse "$@"

####

TIER=`cat ../TIER.md`
BUILD_NUMBER=`cat ../VERSION.md`

function web_sentry_upload () {
  if [ $1 = "webview" ]; then
    # There is no "publish" version for app/webview; it's "published" inside our mobile apps.
    ARTIFACT_FOLDER="$KEYMAN_ROOT/web/build/app/webview/release/"
  elif [ $1 = "browser" ]; then
    ARTIFACT_FOLDER="$KEYMAN_ROOT/web/build/publish/release/"
  fi

  pushd "$ARTIFACT_FOLDER"
  echo "Uploading to Sentry..."
  sentry-cli releases files "$VERSION_GIT_TAG" upload-sourcemaps --strip-common-prefix "@keymanapp/keyman/" --rewrite --ext js --ext map --ext ts || fail "Sentry upload failed."
  echo "Upload successful."
  popd
}

if builder_start_action build; then
  # Build step:  since CI builds start (and should start) from scratch, run the following
  # three actions:
  # - configure:  retrieve all NPM dependencies
  # - clean:      make extra-sure that no prior build products exist.
  #               - also useful when validating this script on a local dev machine!
  # - build:      then do the ACTUAL build.
  # one option:
  # - --ci:       For app/browser, outputs 'release' config filesize profiling logs
  ./build.sh configure clean build --ci

  web_sentry_upload webview
  web_sentry_upload browser

  builder_finish_action success build
fi

if builder_start_action test; then
  # Testing step:  run ALL unit tests, including those of the submodules.

  OPTIONS=
  if ! builder_is_debug_build; then
    OPTIONS=--ci
  fi

  # No --reporter option exists yet for the headless modules.

  $KEYMAN_ROOT/common/web/keyboard-processor/build.sh test $OPTIONS
  $KEYMAN_ROOT/common/web/input-processor/build.sh test $OPTIONS

  ./build.sh test $OPTIONS

  builder_finish_action success test
fi

if builder_start_action post-test; then
  # Always make sure BrowserStack stuff is terminated after the test step.
  # Even if it fails and/or hangs.
  # No matter what.  Otherwise, it may leave a lock-file that breaks a later build!
  ../common/test/resources/test_kill_browserstack.sh

  builder_finish_action success post-test
fi

if builder_start_action validate-size; then
  # Performs the web build product size check as reported on Web test PRs.
  FLAGS=

  if ! builder_is_debug_build; then
    FLAGS=--write-status
  fi

  # Do not use --report when in --debug mode; it sets an error code that is VERY difficult to
  # catch with our set -e & `trap` setup.
  ./src/tools/building/check-build-size.sh $FLAGS

  builder_finish_action success validate-size
fi

if builder_start_action prepare:s.keyman.com; then
  if ! builder_has_option --s.keyman.com; then
    builder_die "--s.keyman.com is unset!"
  fi

  if builder_is_debug_build; then
    # First phase: make sure the s.keyman.com repo is locally-available and up to date.
    pushd "$S_KEYMAN_COM"

    # For testing on a local development machine / a machine with the repo already loaded.
    git checkout master
    git pull
    popd
  fi

  # Second phase:  copy the artifacts over

  # The main build products are expected to reside at the root of this folder.
  BASE_PUBLISH_FOLDER="$S_KEYMAN_COM/kmw/engine/$VERSION"
  echo "FOLDER: $BASE_PUBLISH_FOLDER"
  mkdir -p "$BASE_PUBLISH_FOLDER"

  # s.keyman.com - release-config only.  It's notably smaller, thus far more favorable
  # for distribution via cloud service.
  cp -Rf build/publish/release/* "$BASE_PUBLISH_FOLDER"

  # Third phase: tweak the sourcemaps
  # We can use an alt-mode of Web's sourcemap-root tool for this.
  for sourcemap in "$BASE_PUBLISH_FOLDER/"*.map; do
    node "$KEYMAN_ROOT/web/build/tools/building/sourcemap-root/index.js" null "$sourcemap" --sourceRoot "https://s.keyman.com/kmw/engine/$VERSION/src"
  done

  # Actual construction of the PR will be left to CI-config scripting for now.

  builder_finish_action success prepare:s.keyman.com
fi

# Note:  for now, this command is used to prepare the artifacts used by the download site, but
#        NOT to actually UPLOAD them via rsync or to produce related .download_info files.
if builder_start_action prepare:downloads.keyman.com; then
  UPLOAD_PATH="$KEYMAN_ROOT/web/build/upload/$VERSION"

  # --- First action artifact - the KMW zip file ---
  ZIP="$UPLOAD_PATH/keymanweb-$VERSION.zip"

  mkdir -p "$UPLOAD_PATH"

  # On Windows, we use 7-zip (SEVEN_Z_HOME env var).  On other platforms, we use zip.

  COMPRESS_CMD=
  COMPRESS_ADD=

  # Marc's preference; use $SEVEN_Z_HOME and have the BAs set up with THAT as an env var.
  if [ ! -z "${SEVEN_Z_HOME+x}" ]; then
    COMPRESS_CMD="$SEVEN_Z_HOME/7z"
    COMPRESS_ADD="a -bd -bb0 -r" # add, hide progress, log level 0, recursive
  fi

  if [[ -z "${COMPRESS_CMD}" ]] ; then
    if command -v zip &> /dev/null; then
      # Note:  does not support within-archive renames!
      COMPRESS_CMD=zip
      COMPRESS_ADD="-r"
    else
      builder_die "7z and zip commands are both unavailable"
    fi
  fi

  pushd build/publish
  # Zip both the 'debug' and 'release' configurations together.
  "${COMPRESS_CMD}" $COMPRESS_ADD $ZIP *
  popd

  # --- Second action artifact - the 'static' folder (hosted user testing on downloads.keyman.com) ---

  echo ""
  echo "Building \`static/\` folder for long-term hosting of testing resources..."
  STATIC="$UPLOAD_PATH/static"
  mkdir -p "$STATIC"

  mkdir -p "$STATIC/build"
  cp -rf build/app    "$STATIC/build/app"
  cp -rf build/engine "$STATIC/build/engine"
  cp -rf build/tools  "$STATIC/build/tools"
  # avoid build/upload, since that's the folder we're building!

  cp -f index.html "$STATIC/index.html"

  mkdir -p "$STATIC/src/tools"
  cp -rf src/tools/testing "$STATIC/src/tools/testing"
  cp -rf src/test          "$STATIC/src/test"
  cp -rf src/samples       "$STATIC/src/samples"

  builder_finish_action success prepare:downloads.keyman.com
fi