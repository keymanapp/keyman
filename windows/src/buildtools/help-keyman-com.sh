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

if [ ! -d "$HELP_KEYMAN_COM/products/desktop/" ]; then
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

  #
  # Note: release/packages which contain multiple keyboards should also have the keyboards
  # as separate entries in the release/ folder. This means we may have a combined package
  # help file as well as a per-keyboard help file. It is acceptable in this situation to
  # make the combined help file link to the per-keyboard help files.
  #

  #
  # Copy all files in that folder, according to the current
  # version of the keyboard, to help.keyman.com/keyboard/<id>/<version>/
  #

  local helppath=$KEYMANROOT/../bin/help/php/desktop

  #
  # Look for help source folder.
  #

  if [[ ! -d "$helppath" ]]; then
    echo "${t_yel}Warning: The source path $helppath does not exist${t_end}"
    return 0
  fi

  local dstpath="$HELP_KEYMAN_COM/products/desktop/$VERSION_RELEASE/docs"

  mkdir -p "$dstpath"

  rm -rf "$dstpath/*"
  cp -r "$helppath"/* "$dstpath/"
}

#
# Commit and push to the help.keyman.com repo
# TODO: turn this into a pull request
#

function commit_and_push {
  echo "Committing and pushing updated Keyman for Windows documentation"

  pushd $HELP_KEYMAN_COM
  git config user.name "Keyman Build Server"
  git config user.email "keyman-server@users.noreply.github.com"
  git add products/desktop/$VERSION_RELEASE/docs || return 1
  git diff --cached --no-ext-diff --quiet --exit-code && {
    # if no changes then don't do anything.
    echo "No changes to commit"
    popd
    return 0
  }

  echo "changes added to cache...>>>"
  git commit -m "Keyman for Windows help deployment (automatic)" || return 1
  git pull origin master || return 1
  git push origin master || return 1
  popd

  echo "Push to help.keyman.com complete"

  return 0
}

#
# Main
#

upload_keyman_for_windows_help || exit 1
commit_and_push || exit 1
