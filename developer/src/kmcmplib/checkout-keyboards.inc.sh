#!/usr/bin/env bash

#
# Look for the keyboards repo, either at $KEYBOARDS_ROOT, or, if that is not
# set, prepare to clone from GitHub. It will always be cloned to ./keyboards/
#
function locate_keyboards_repo() {
  if [[ ! -f "$THIS_SCRIPT_PATH/tests/keyboards_commit_ref.txt" ]]; then
    builder_die "keyboards_commit_ref.txt does not exist, run prep.sh"
  fi

  if [[ ! -d "$THIS_SCRIPT_PATH/tests/fixtures/keyboards-repo" ]]; then
    builder_die "fixtures/keyboards-repo folder does not exist, run prep.sh"
  fi

  if [[ -z "${KEYBOARDS_ROOT+x}" ]]; then
    # KEYBOARDS_ROOT is not set, so we'll clone from GitHub
    export KEYBOARDS_SOURCE_REPO=https://github.com/keymanapp/keyboards
    builder_echo warning "\$KEYBOARDS_ROOT variable not set, using $KEYBOARDS_SOURCE_REPO"
    builder_echo warning "For faster test builds, setup a local clone of that repo and set"
    builder_echo warning "\$KEYBOARDS_ROOT to point to it"
  else
    builder_echo "\$KEYBOARDS_ROOT variable is $KEYBOARDS_ROOT"
    export KEYBOARDS_SOURCE_REPO="$KEYBOARDS_ROOT"
  fi
}

#
# Shallow-clone of keyboards repo at commit found in keyboards_commit_ref.txt
#
function checkout_keyboards() {
  local KEYBOARDS_COMMIT=$(cat $THIS_SCRIPT_PATH/tests/keyboards_commit_ref.txt)

  mkdir -p "$THIS_SCRIPT_PATH/tests/keyboards"
  pushd "$THIS_SCRIPT_PATH/tests/keyboards" > /dev/null

  if [[ ! -f "build.sh" ]]; then
    # We'll have to init the folder and clone the repo
    builder_echo "Checking out keyboards repository at $THIS_SCRIPT_PATH/tests/keyboards"
    git init
    git remote add origin "$KEYBOARDS_SOURCE_REPO"
  fi

  # Make sure we have fetched the commit, as it may have changed
  # from a previous build
  builder_echo "Fetching commit $KEYBOARDS_COMMIT in repo at $THIS_SCRIPT_PATH/tests/keyboards"
  git fetch --depth=1 --no-tags origin "$KEYBOARDS_COMMIT"
  git reset --hard "$KEYBOARDS_COMMIT"

  popd > /dev/null
}
