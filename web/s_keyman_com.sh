#!/bin/bash

#
# Prevents 'clear' on exit of mingw64 bash shell
#
SHLVL=0

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../resources/build/build-utils.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/jq.inc.sh"

#
# In this script we update the s.keyman.com git repo and copy the Keyman Web release
# to s.keyman.com/kmw/engine. This should be run after a full build.
#

#
# These are passed via environment:
#
# HELP_KEYMAN_COM = the home of the help.keyman.com repository
#
# That repo must have push to origin configured and logged in.
# Note: GitHub API only available via HTTPS, and not SSH
# https://github.com/github/hub/issues/1644#issuecomment-359002547
#

if [ -z ${S_KEYMAN_COM+x} ]; then
  >&2 echo "Not uploading KeymanWeb release: must set S_KEYMAN_COM in environment."
  exit 1
fi

if [ ! -d "$S_KEYMAN_COM/kmw/engine/" ]; then
  >&2 echo "S_KEYMAN_COM path ($S_KEYMAN_COM) does not appear to be valid."
  exit 1
fi

function display_usage ( ) {
    echo "Usage: $0"
    echo
    echo "  Push KeymanWeb release to s.keyman.com/kmw/engine"
    exit 1
}

#
# Define terminal colours
#

if [ -t 2 ]; then
  t_red=$'\e[1;31m'
  t_grn=$'\e[1;32m'
  t_yel=$'\e[1;33m'
  t_blu=$'\e[1;34m'
  t_mag=$'\e[1;35m'
  t_cyn=$'\e[1;36m'
  t_end=$'\e[0m'
fi

##
## Upload KeymanWeb release s.keyman.com
## Also remap sourcemap paths
##
function upload_keyman_web {

  local kmwpath="$KEYMAN_ROOT/web/release/web"
  local dstpath="$S_KEYMAN_COM/kmw/engine/$VERSION/src"

  #
  # Look for KeymanWeb release folder.
  #

  if [[ ! -d "$kmwpath" ]]; then
    echo "${t_yel}Warning: The KeymanWeb release path $kmwpath does not exist${t_end}"
    exit 1
  fi

  mkdir -p "$dstpath"

  echo "Copy KeymanWeb artifacts to s.keyman.com/kmw/engine/$VERSION"
  rm -rf "$dstpath/*"
  cp -rf "$kmwpath"/* "$dstpath/"
}

##
## Remap sourcemap paths
##
function remap_sourcemap_paths {
  local dstpath="$S_KEYMAN_COM/kmw/engine/$VERSION/src"

  cd $dstpath
  for mapfile in *.map; do
    echo "Remapping sourcempath paths for: $mapfile"
    sourceRoot="https://s.keyman.com/kmw/engine/$VERSION/src/"
    jqi ". + {sourceRoot: \"$sourceRoot\"}" $mapfile
  done
}


#
# Commit and push to the help.keyman.com repo
# Creates a pull request with the 'auto' label
# Which a GitHub action will watch for in order
# to automatically process and merge it
#

function commit_and_push {
  echo "Committing and pushing KeymanWeb"
  cd "$KEYMAN_ROOT/web/"
  pushd "$S_KEYMAN_COM"

  if [ ! -z "${TEAMCITY_VERSION-}" ]; then
    git config user.name "Keyman Build Server"
    git config user.email "keyman-server@users.noreply.github.com"
  fi

  local branchname="auto/kmw-release-$VERSION"
  local modifiedfiles="kmw/engine/$VERSION"

  local basebranch="master"

  git checkout -b $branchname $basebranch
  git add $modifiedfiles || return 1
  git diff --cached --no-ext-diff --quiet --exit-code && {
    # if no changes then don't do anything.
    echo "No changes to commit"
    popd
    return 0
  }

  echo "changes added to cache...>>>"
  local commitmessage="auto: KeymanWeb release $VERSION"
  git commit -m "$commitmessage" || return 1
  git push origin $branchname || return 1
  hub pull-request -b $basebranch -l auto -m "$commitmessage" || return 1
  popd

  echo "Push to s.keyman.com complete"

  return 0
}

#
# Main
#

upload_keyman_web
remap_sourcemap_paths
#commit_and_push
