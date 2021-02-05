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
# PLATFORM = platform of the Keyman help to upload
#
# That repo must have push to origin configured and logged in
#

if [ -z ${HELP_KEYMAN_COM+x} ]; then
  >&2 echo "Not uploading documentation: must set HELP_KEYMAN_COM in environment."
  exit 1
fi

if [ -z ${PLATFORM} ]; then
  >&2 echo "No uploading documentation: must set PLATFORM in environment."
  exit 1
fi

if [ ! -d "$HELP_KEYMAN_COM/products/" ]; then
  >&2 echo "HELP_KEYMAN_COM path ($HELP_KEYMAN_COM) does not appear to be valid."
  exit 1
fi

#
# Environment
#

echo "Uploading Keyman for $PLATFORM documentation to help.keyman.com"

#
# Uploading Keyman documentation
#

##
## Upload documentation updates to help.keyman.com
## Paths depend on $PLATFORM
##
function upload_keyman_help {

  local helppath
  local dstpath

  case $PLATFORM in
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
      echo "Invalid PLATFORM ${PLATFORM}"
      exit 1
    esac

  #
  # Look for help source folder.
  #

  if [[ ! -d "$helppath" ]]; then
    echo "${t_yel}Warning: The source path $helppath does not exist${t_end}"
    return 0
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
  echo "Committing and pushing updated Keyman for Windows documentation"

  pushd $HELP_KEYMAN_COM

  if [! -z "${TEAMCITY_VERSION-}" ]; then
    git config user.name "Keyman Build Server"
    git config user.email "keyman-server@users.noreply.github.com"
  fi

  local branchname="auto/$PLATFORM-help-$VERSION_WITH_TAG"
  local modifiedfiles="$HELP_KEYMAN_COM/products/$PLATFORM/$VERSION_RELEASE"

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
  local commitmessage="auto: Keyman for $PLATFORM help deployment"
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

upload_keyman_help || exit 1
commit_and_push || exit 1
