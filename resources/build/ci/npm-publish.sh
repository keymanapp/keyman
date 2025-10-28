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

builder_describe \
  "Publish @keymanapp packages to NPM" \
  "pack              Pack NPM packages to a .tgz for verification" \
  "publish           Publish NPM packages to the NPM Registry" \
  "--dry-run         Don't publish/pack anything, just dry run"

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

echo "Keyman root: ${KEYMAN_ROOT}"

function do_pack() {
  local npm_package_path
  for npm_package_path in "${PACKAGES[@]}"; do
    builder_heading "Building and packing ${npm_package_path}"
    "${KEYMAN_ROOT}/${npm_package_path}/build.sh build test"
    ci_publish_npm_package pack "${npm_package_path}"
  done
}

function do_publish() {
  local npm_package_path

  if [[ $KEYMAN_VERSION_ENVIRONMENT =~ local|test ]] && ! builder_has_option --dry-run; then
    builder_die "publish must use --dry-run flag for local or test builds"
  fi

  for npm_package_path in "${PACKAGES[@]}"; do
    builder_heading "Building and publishing ${npm_package_path}"
    "${KEYMAN_ROOT}/${npm_package_path}/build.sh build test"
    ci_publish_npm_package publish "${npm_package_path}"
  done
}

builder_run_action pack    do_pack
builder_run_action publish do_publish
