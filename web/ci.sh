#!/usr/bin/env bash
#
# Provides a platform-independent implementation of the build steps used for CI when
# building the Keyman Engine for Web.
#

# set -x

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "${KEYMAN_ROOT}/resources/shellHelperFunctions.sh"
. "${KEYMAN_ROOT}/resources/build/ci/pull-requests.inc.sh"

# This script runs from its own folder
cd "${THIS_SCRIPT_PATH}"

# ################################ Main script ################################

S_KEYMAN_COM=

builder_describe "CI processes for Keyman Engine for Web releases (KMW)." \
  "@/web/src/tools/building/sourcemap-root prepare:s.keyman.com" \
  "build" \
  "test                         Runs all unit tests" \
  "post-test                    Runs post-test cleanup. Should be run even if a prior step fails" \
  "validate-size                Runs the build-size comparison check" \
  "prepare                      Prepare upload artifacts for specified target(s)" \
  ":s.keyman.com                Builds artifacts for s.keyman.com" \
  ":downloads.keyman.com        Builds artifacts for downloads.keyman.com" \
  "--s.keyman.com=S_KEYMAN_COM  Sets the root location of a checked-out s.keyman.com repo"

builder_parse "$@"

####

TIER=$(cat ../TIER.md)
BUILD_NUMBER=$(cat ../VERSION.md)

function web_sentry_upload () {
  if [[ -z "${SENTRY_ORG:-}" ]] || [[ -z "${SENTRY_PROJECT:-}" ]]; then
    echo "Skipping Sentry upload: SENTRY_ORG and/or SENTRY_PROJECT are unset."
    return
  fi

  echo "Uploading $1 to Sentry..."

  # --strip-common-prefix does not take an argument, unlike --strip-prefix.  It auto-detects
  # the most common prefix instead.
  sentry-cli releases files "${VERSION_GIT_TAG}" upload-sourcemaps --strip-common-prefix "$1" \
    --rewrite --ext js --ext map --ext ts
  echo "Upload successful."
}

function build_action() {
  # Build step:  since CI builds start (and should start) from scratch, run the following
  # three actions:
  # - configure:  retrieve all NPM dependencies
  # - clean:      make extra-sure that no prior build products exist.
  #               - also useful when validating this script on a local dev machine!
  # - build:      then do the ACTUAL build.
  # one option:
  # - --ci:       For app/browser, outputs 'release' config filesize profiling logs
  ./build.sh configure clean build --ci

  # Upload the sentry-configuration engine used by the mobile apps to sentry
  # Also, clean 'em first.
  for sourcemap in "${KEYMAN_ROOT}/web/src/engine/sentry-manager/build/lib/"*.map; do
    node "${KEYMAN_ROOT}/web/build/tools/building/sourcemap-root/index.js" null "${sourcemap}" --clean
  done
  web_sentry_upload "${KEYMAN_ROOT}/web/src/engine/sentry-manager/build/lib/"

  # And, of course, the main build-products too
  web_sentry_upload "${KEYMAN_ROOT}/web/build/app/webview/release/"
  web_sentry_upload "${KEYMAN_ROOT}/web/build/publish/release/"
}

function test_action() {
  # Testing step:  run ALL unit tests, including those of the submodules.

  OPTIONS=
  if ! builder_is_debug_build; then
    OPTIONS=--ci
  fi

  # No --reporter option exists yet for the headless modules.

  "${KEYMAN_ROOT}/web/src/engine/keyboard/build.sh" test ${OPTIONS}
  "${KEYMAN_ROOT}/web/src/engine/osk/gesture-processor/build.sh" test ${OPTIONS}

  ./build.sh test ${OPTIONS}
}

function post_test_action() {
  # Always make sure BrowserStack stuff is terminated after the test step.
  # Even if it fails and/or hangs.
  # No matter what.  Otherwise, it may leave a lock-file that breaks a later build!
  ../common/test/resources/test_kill_browserstack.sh
}

function validate_size_action() {
  # Performs the web build product size check as reported on Web test PRs.
  FLAGS=

  if ! builder_is_debug_build; then
    FLAGS=--write-status
  fi

  # Do not use --report when in --debug mode; it sets an error code that is VERY difficult to
  # catch with our set -e & `trap` setup.
  ./src/tools/building/check-build-size.sh $FLAGS
}

function prepare_s_keyman_com_action() {
  if ! builder_has_option --s.keyman.com; then
    builder_die "--s.keyman.com is unset!"
  fi

  if builder_is_debug_build; then
    # First phase: make sure the s.keyman.com repo is locally-available and up to date.
    pushd "${S_KEYMAN_COM}"

    # For testing on a local development machine / a machine with the repo already loaded.
    git checkout master
    git pull
    popd
  fi

  # Second phase:  copy the artifacts over

  # The main build products are expected to reside at the root of this folder.
  BASE_PUBLISH_FOLDER="${S_KEYMAN_COM}/kmw/engine/${VERSION}"
  echo "FOLDER: ${BASE_PUBLISH_FOLDER}"
  mkdir -p "${BASE_PUBLISH_FOLDER}"

  # s.keyman.com - release-config only.  It's notably smaller, thus far more favorable
  # for distribution via cloud service.
  cp -Rf build/publish/release/* "${BASE_PUBLISH_FOLDER}"

  # Third phase: tweak the sourcemaps
  # We can use an alt-mode of Web's sourcemap-root tool for this.
  for sourcemap in "${BASE_PUBLISH_FOLDER}/"*.map; do
    node "${KEYMAN_ROOT}/web/build/tools/building/sourcemap-root/index.js" null "${sourcemap}" --sourceRoot "https://s.keyman.com/kmw/engine/${VERSION}/src"
  done

  # Construct the PR
  echo "Committing and pushing KeymanWeb release ${VERSION} to s.keyman.com"

  ci_add_files "${S_KEYMAN_COM}" "kmw/engine/${VERSION}"
  if ! ci_repo_has_cached_changes "${S_KEYMAN_COM}"; then
    builder_die "No release was added to s.keyman.com, something went wrong"
  fi

  ci_open_pull_request "${S_KEYMAN_COM}" auto/keymanweb/release "auto: KeymanWeb release ${VERSION}"
}

# Note:  for now, this command is used to prepare the artifacts used by the download site, but
#        NOT to actually UPLOAD them via rsync or to produce related .download_info files.
function prepare_downloads_keyman_com_action() {
  UPLOAD_PATH="${KEYMAN_ROOT}/web/build/upload/${VERSION}"

  # --- First action artifact - the KMW zip file ---
  ZIP="${UPLOAD_PATH}/keymanweb-${VERSION}.zip"

  mkdir -p "${UPLOAD_PATH}"

  # On Windows, we use 7-zip (SEVEN_Z_HOME env var).  On other platforms, we use zip.

  COMPRESS_CMD=
  COMPRESS_ADD=

  # Marc's preference; use $SEVEN_Z_HOME and have the BAs set up with THAT as an env var.
  if [[ ! -z "${SEVEN_Z_HOME+x}" ]]; then
    COMPRESS_CMD="${SEVEN_Z_HOME}/7z"
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
  "${COMPRESS_CMD}" ${COMPRESS_ADD} ${ZIP }*
  popd

  # --- Second action artifact - the 'static' folder (hosted user testing on downloads.keyman.com) ---

  echo ""
  echo "Building \`static/\` folder for long-term hosting of testing resources..."
  STATIC="${UPLOAD_PATH}/static"
  mkdir -p "${STATIC}"

  mkdir -p "${STATIC}/build"
  cp -rf build/app    "${STATIC}/build/app"
  cp -rf build/engine "${STATIC}/build/engine"
  cp -rf build/tools  "${STATIC}/build/tools"
  # avoid build/upload, since that's the folder we're building!

  cp -f index.html "${STATIC}/index.html"

  mkdir -p "${STATIC}/src/tools"
  cp -rf src/tools/testing "${STATIC}/src/tools/testing"
  cp -rf src/test          "${STATIC}/src/test"
  cp -rf src/samples       "${STATIC}/src/samples"
}

builder_run_action  build                         build_action
builder_run_action  test                          test_action
builder_run_action  post-test                     post_test_action
builder_run_action  validate-size                 validate_size_action
builder_run_action  prepare:s.keyman.com          prepare_s_keyman_com_action
builder_run_action  prepare:downloads.keyman.com  prepare_downloads_keyman_com_action
