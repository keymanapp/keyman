#!/usr/bin/env bash
#
# Keyman is copyright (C) SIL Global. MIT License.
#
# Publish all the @keymanapp packages listed in npm-packages.inc.sh
#
# If the `--dry-run` option is available and specified as a command-line
# parameter, will do a dry run
#
# Note that `package.json` will be dirty after this command, as the `version`
# field will be added to it, and @keymanapp dependency versions will also be
# modified. This change should not be committed to the repository.
#
# If `publish` is called:
#  * then ci_publish_npm publishes to the public registry
#  * else ci_publish_npm creates a local tarball which can be used to test
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/ci/ci-publish.inc.sh"
. "$KEYMAN_ROOT/resources/build/ci/npm-packages.inc.sh"
. "$KEYMAN_ROOT/resources/build/minimum-versions.inc.sh"

builder_describe \
  "Publish @keymanapp packages to NPM" \
  "configure         Install required modules for build on GHA" \
  "pack              Pack NPM packages to a .tgz for verification" \
  "publish           Publish NPM packages to the NPM Registry" \
  "--dry-run         Don't publish/pack anything, just dry run"

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

function do_pack() {
  do_package_builds
  do_pack_or_publish pack
}

function do_package_builds() {
  local npm_package_path
  for npm_package_path in "${PACKAGES[@]}"; do
    builder_heading "Building ${npm_package_path}"
    "${KEYMAN_ROOT}/${npm_package_path}/build.sh" build test
  done
}

function inject_sourcemaps() {
  local SOURCEMAP_PATHS
  SOURCEMAP_PATHS=( "${PACKAGES[@]}" )
  SOURCEMAP_PATHS=( "${SOURCEMAP_PATHS[@]/%//build}" )

  builder_heading "Injecting sourcemaps"

  pushd "${KEYMAN_ROOT}"

  sentry-cli sourcemaps inject \
    --org keyman \
    --project keyman-developer \
    --release "$KEYMAN_VERSION_GIT_TAG"  \
    "${SOURCEMAP_PATHS[@]}"

  if [[ "${GHA_TEST_BUILD+x}" == false ]]; then
    builder_heading "Uploading sourcemaps"

    sentry-cli sourcemaps upload \
      --no-dedupe \
      --org keyman \
      --project keyman-developer \
      --release "$KEYMAN_VERSION_GIT_TAG"  \
      --ext js --ext mjs --ext ts --ext map \
      "${SOURCEMAP_PATHS[@]}"
  else
    builder_heading "Skipping upload of sourcemaps (test build)"
  fi

  popd
}

function do_pack_or_publish() {
  local npm_action="$1"
  local npm_package_path

  inject_sourcemaps

  for npm_package_path in "${PACKAGES[@]}"; do
    builder_heading "npm ${npm_action} ${npm_package_path}"
    ci_publish_npm_package "${npm_action}" "${npm_package_path}"
  done
}

function do_publish() {
  if [[ $KEYMAN_VERSION_ENVIRONMENT =~ local|test ]] && ! builder_has_option --dry-run; then
    builder_die "publish must use --dry-run flag for local or test builds"
  fi

  do_package_builds
  do_pack_or_publish publish
}

function install_emscripten() {
  builder_heading "Installing emscripten"

  local EMSDK_TEMP
  EMSDK_TEMP=$(mktemp -d)
  pushd "${EMSDK_TEMP}"
  git clone https://github.com/emscripten-core/emsdk.git
  cd emsdk
  ./emsdk install "${KEYMAN_MIN_VERSION_EMSCRIPTEN}"
  ./emsdk activate "${KEYMAN_MIN_VERSION_EMSCRIPTEN}"
  cd upstream/emscripten
  npm install
  echo "EMSCRIPTEN_BASE=$(pwd)" >> $GITHUB_ENV
  EMSCRIPTEN_BASE="$(pwd)"
  export EMSCRIPTEN_BASE
  popd
}

function install_meson() {
  builder_heading "Installing meson"

  builder_echo "Starting apt-get update + install meson"
  sudo apt-get update
  sudo apt-get install meson
  echo "meson version:"
  meson -v
}

function install_sentry_cli() {
  builder_heading "Installing sentry-cli"

  curl -sL https://sentry.io/get-cli/ | SENTRY_CLI_VERSION="2.57.0" sh
  sentry-cli --version
}

function do_configure() {
  install_meson
  install_emscripten
  install_sentry_cli
}

builder_run_action configure do_configure
builder_run_action pack      do_pack
builder_run_action publish   do_publish
