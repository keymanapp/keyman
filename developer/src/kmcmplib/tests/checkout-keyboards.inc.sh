#!/usr/bin/env bash

#
# Look for the keyboards repo, either at $KEYBOARDS_ROOT, or, if that is not
# set, assume that it will be checked out at ./keyboards/
#
function locate_keyboards_repo() {
  local LKR_THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
  local LKR_THIS_SCRIPT_PATH="$(dirname "$LKR_THIS_SCRIPT")"

  if [[ ! -f "$LKR_THIS_SCRIPT_PATH/keyboards_commit_ref.txt" ]]; then
    echo "keyboards_commit_ref.txt does not exist, run prep.sh"
    exit 1
  fi

  if [[ ! -d "$LKR_THIS_SCRIPT_PATH/fixtures" ]]; then
    echo "fixtures folder does not exist, run prep.sh"
    exit 1
  fi

  if [[ -z "${KEYBOARDS_ROOT+x}" ]]; then
    # KEYBOARDS_ROOT is not set, so using temporary 'keyboards' folder"
    export KEYBOARDS_ROOT="$LKR_THIS_SCRIPT_PATH/keyboards"
    echo "\$KEYBOARDS_ROOT variable not set, using $KEYBOARDS_ROOT"
  else
    echo "\$KEYBOARDS_ROOT variable is $KEYBOARDS_ROOT"
  fi
}

#
# Shallow-clone of keyboards repo at commit found in keyboards_commit_ref.txt
#
function checkout_keyboards() {
  local LKR_THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
  local LKR_THIS_SCRIPT_PATH="$(dirname "$LKR_THIS_SCRIPT")"

  local KEYBOARDS_COMMIT=$(cat $LKR_THIS_SCRIPT_PATH/keyboards_commit_ref.txt)

  if [[ ! -f "$KEYBOARDS_ROOT/build.sh" ]]; then
    # We'll have to init the folder and clone the repo
    echo "Checking out keyboards repository at $KEYBOARDS_ROOT"
    pushd "$KEYBOARDS_ROOT" > /dev/null
    git init
    git remote add origin https://github.com/keymanapp/keyboards
    git fetch --depth=1 --no-tags origin $KEYBOARDS_COMMIT
    popd "$KEYBOARDS_ROOT" > /dev/null
  fi

  if [[ "$(git rev-parse head)" != "$KEYBOARDS_COMMIT" ]]; then
    echo "Checking out commit $KEYBOARDS_COMMIT in repo at $KEYBOARDS_ROOT"
    pushd "$KEYBOARDS_ROOT" > /dev/null
    # assuming keyboards repo has the right commit
    git reset --hard $(cat "$LKR_THIS_SCRIPT_PATH/keyboards_commit_ref.txt")
    popd > /dev/null
  fi
}
