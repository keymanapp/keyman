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
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

#
# Allows us to check for existence of subfolders in help/
#
shopt -s nullglob

#
# In this script we update the git repo and copy the Keyman for Windows
# documentation over. This should be run after a full build.
#

#
# These are passed via environment:
#
# HELP_KEYMAN_COM = the home of the help.keyman.com repository
#
# That repo must have push to origin configured and logged in
#

if [ -z ${HELP_KEYMAN_COM+x} ]; then
  >&2 echo "Not uploading documentation: must set HELP_KEYMAN_COM in environment."
  exit 1
fi

if [ ! -d "$HELP_KEYMAN_COM/products/" ]; then
  >&2 echo "HELP_KEYMAN_COM path ($HELP_KEYMAN_COM) does not appear to be valid."
  exit 1
fi

#
# Environment
#

KEYMANROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/.."

# . "$KEYBOARDROOT/servervars.sh"
# . "$KEYBOARDROOT/resources/util.sh"

echo "Uploading Keyman for Windows documentation to help.keyman.com"

#
# Uploading Keyman for Windows documentation
#

##
## Upload documentation updates to help.keyman.com
##
function upload_keyman_for_windows_help {

  local helppath=$KEYMANROOT/../bin/help/md/desktop

  #
  # Look for help source folder.
  #

  if [[ ! -d "$helppath" ]]; then
    echo "${t_yel}Warning: The source path $helppath does not exist${t_end}"
    return 0
  fi

  local dstpath="$HELP_KEYMAN_COM/products/windows/$VERSION_RELEASE"

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
  git config user.name "Keyman Build Server"
  git config user.email "keyman-server@users.noreply.github.com"
  git checkout -b auto/windows-help-$VERSION_WITH_TAG master
  git add products/windows/$VERSION_RELEASE || return 1
  git diff --cached --no-ext-diff --quiet --exit-code && {
    # if no changes then don't do anything.
    echo "No changes to commit"
    popd
    return 0
  }

  echo "changes added to cache...>>>"
  git commit -m "auto: Keyman for Windows help deployment" || return 1
  git push origin auto/windows-help-$VERSION_WITH_TAG || return 1
  hub pull-request -l auto -m "auto: Keyman for Windows help deployment" || return 1
  popd

  echo "Push to help.keyman.com complete"

  return 0
}

#
# Main
#

upload_keyman_for_windows_help || exit 1
commit_and_push || exit 1
