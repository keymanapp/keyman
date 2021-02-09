#!/bin/bash

#
# Prevents 'clear' on exit of mingw64 bash shell
#
SHLVL=0

set -e
set -u

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

#
# Allows us to check for existence of subfolders in help/
#
shopt -s nullglob

#
# In this script we update the git repo and copy the Keyman for [Platform]
# documentation over. This should be run after a full build.
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

if [ -z ${HELP_KEYMAN_COM+x} ]; then
  >&2 echo "Not uploading documentation: must set HELP_KEYMAN_COM in environment."
  exit 1
fi

if [ ! -d "$HELP_KEYMAN_COM/products/" ]; then
  >&2 echo "HELP_KEYMAN_COM path ($HELP_KEYMAN_COM) does not appear to be valid."
  exit 1
fi

function display_usage {
  echo "Usage: $0 [platform]"
  echo "       $0 -help"
  echo
  echo "  platform should be one of: android, ios, linux, mac, windows."
  echo "  -help               displays this screen and exits"
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

#
# Uploading Keyman documentation
#

##
## Upload documentation updates to help.keyman.com
## Paths depend on $platform
##
function upload_keyman_help {

  local helppath
  local dstpath

  case $platform in
    android)
      helppath=$KEYMAN_ROOT/android/help
      dstpath="$HELP_KEYMAN_COM/products/android/$VERSION_RELEASE"
      ;;
    ios)
      helppath=$KEYMAN_ROOT/ios/help
      dstpath="$HELP_KEYMAN_COM/products/iphone-and-ipad/$VERSION_RELEASE"
      ;;
    linux)
      helppath=$KEYMAN_ROOT/linux/help
      dstpath="$HELP_KEYMAN_COM/products/linux/$VERSION_RELEASE"
      ;;
    mac)
      helppath=$KEYMAN_ROOT/mac/help
      dstpath="$HELP_KEYMAN_COM/products/mac/$VERSION_RELEASE"
      ;;
    windows)
      helppath=$KEYMAN_ROOT/windows/bin/help/md/desktop
      dstpath="$HELP_KEYMAN_COM/products/windows/$VERSION_RELEASE"
      ;;
    *)
      display_usage
    esac

  #
  # Look for help source folder.
  #

  if [[ ! -d "$helppath" ]]; then
    echo "${t_yel}Warning: The source path $helppath does not exist${t_end}"
    exit 1
  fi

  mkdir -p "$dstpath"

  rm -rf "$dstpath/*"
  cp -r "$helppath"/* "$dstpath/"
}

#
# Commit and push to the help.keyman.com repo
# Creates a pull request with the 'auto' label
# Which a GitHub action will watch for in order
# to automatically process and merge it
#

function commit_and_push {
  echo "Committing and pushing updated Keyman for $platform documentation"

  pushd $HELP_KEYMAN_COM

  if [ ! -z "${TEAMCITY_VERSION-}" ]; then
    git config user.name "Keyman Build Server"
    git config user.email "keyman-server@users.noreply.github.com"
  fi

  local branchname="auto/$platform-help-$VERSION_WITH_TAG"
  local modifiedfiles="$HELP_KEYMAN_COM/products/$platform/$VERSION_RELEASE"

  # Base branch depends on the tier
  local basebranch="master"
  if [ "$TIER" == "alpha" ] || [ "$TIER" == "beta" ]; then
      basebranch="staging"
  fi

  git checkout -b $branchname $basebranch
  git add $modifiedfiles || return 1
  git diff --cached --no-ext-diff --quiet --exit-code && {
    # if no changes then don't do anything.
    echo "No changes to commit"
    popd
    return 0
  }

  echo "changes added to cache...>>>"
  local commitmessage="auto: Keyman for $platform help deployment"
  git commit -m "$commitmessage" || return 1
  git push origin $branchname || return 1
  hub pull-request -b $basebranch -l auto -m "$commitmessage" || return 1
  popd

  echo "Push to help.keyman.com complete"

  return 0
}

#
# Main
#

platform=

# Process command-line arguments
while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    -help|-h)
      display_usage
      exit
      ;;
    android | ios | linux | linux | mac | windows)
      platform=$key
      ;;
    *)
      echo "$0: invalid option: $key"
      display_usage
  esac
  shift # past the processed argument
done

if [ -z ${platform} ]; then
  display_usage
fi

echo "Uploading Keyman for $platform documentation to help.keyman.com"

upload_keyman_help || exit 1
commit_and_push || exit 1
