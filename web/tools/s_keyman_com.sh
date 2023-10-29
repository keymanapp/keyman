#!/usr/bin/env bash
#
# TODO: is this script still necessary or is /web/ci.sh a complete replacement?
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/jq.inc.sh"
. "$KEYMAN_ROOT/resources/build/ci/pull-requests.inc.sh"

#
# In this script we update the s.keyman.com git repo and copy the Keyman Web release
# to s.keyman.com/kmw/engine. This should be run after a full build.
#

#
# These are passed via environment:
#
# S_KEYMAN_COM = the home of the s.keyman.com repository
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
##
function upload_keyman_web {

  local kmwpath="$KEYMAN_ROOT/web/release/web"
  local dstpath="$S_KEYMAN_COM/kmw/engine/$VERSION/"

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
  local dstpath="$S_KEYMAN_COM/kmw/engine/$VERSION/"

  cd $dstpath
  for mapfile in *.map; do
    echo "Remapping sourcemap paths for: $mapfile"
    sourceRoot="https://s.keyman.com/kmw/engine/$VERSION/src/"
    jqi ". + {sourceRoot: \"$sourceRoot\"}" $mapfile
  done
}

#
# Main
#

upload_keyman_web
remap_sourcemap_paths

echo "Committing and pushing KeymanWeb release $VERSION to s.keyman.com"

ci_add_files "$S_KEYMAN_COM" "kmw/engine/$VERSION"
if ! ci_repo_has_cached_changes "$S_KEYMAN_COM"; then
  builder_die "No release was added to s.keyman.com, something went wrong"
fi

ci_open_pull_request "$S_KEYMAN_COM" auto/keymanweb/release "auto: KeymanWeb release $VERSION"
