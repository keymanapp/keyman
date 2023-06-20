#!/usr/bin/env bash
#
# Generates fixtures for testing against the legacy compiler
#
# Assumes that the
#

# Exit on command failure and when using unset variables:
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

cd "$THIS_SCRIPT_PATH"

builder_describe \
"Generates fixtures for testing against the legacy compiler.

  Developer must be built locally and binaries should be in developer/bin/.

  Assumes that the keyboards repository exists and is clean. Will record the
  commit hash to allow corresponding CI builds to use the same commit hash
  for validation." \
  clean \
  build \
  "--keyboards-root,-k=KEYBOARDS_ROOT    Path to keyboards repository"

builder_parse "$@"

if ! builder_has_option --keyboards-root; then
  builder_die "--keyboards-root option is required"
fi

if [[ ! -d "$KEYBOARDS_ROOT/.git" ]] || [[ ! -f "$KEYBOARDS_ROOT/tools/regression.sh" ]]; then
  builder_die "Path $KEYBOARDS_ROOT does not appear to be a valid keyboard repository"
fi

if builder_start_action clean; then
  rm -rf "$THIS_SCRIPT_PATH/fixtures/keyboards-repo"
  rm -f "$THIS_SCRIPT_PATH/keyboards_commit_ref.txt"
  builder_finish_action success clean
fi

if builder_start_action build; then
  # yes, remove existing fixtures before build so we don't end up with stale fixtures
  rm -rf "$THIS_SCRIPT_PATH/fixtures/keyboards-repo"

  # record the revision of the keyboards repo
  pushd "$KEYBOARDS_ROOT" > /dev/null
  git rev-parse head > "$THIS_SCRIPT_PATH/keyboards_commit_ref.txt"
  popd > /dev/null

  "$KEYBOARDS_ROOT/tools/regression.sh" --use-legacy-compiler --local "$KEYMAN_ROOT/developer/bin/" --output "$THIS_SCRIPT_PATH/fixtures/keyboards-repo/"

  cd "$THIS_SCRIPT_PATH"

  # We don't need visual keyboards
  rm ./fixtures/keyboards-repo/*.kvk
  # Nor do we need kmw outputs
  rm ./fixtures/keyboards-repo/*.js
  # Finally, we remove any keyboards over 500kb, as we won't automatically test them for now
  # TODO: consider if we want to keep those large keyboards in the future
  find ./fixtures/keyboards-repo/ -size +500k -exec rm {} \+
  builder_finish_action success build
fi