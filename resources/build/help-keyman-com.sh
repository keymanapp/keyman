#!/usr/bin/env bash
# TODO: rewrite this script to builder reqs
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/ci/pull-requests.inc.sh"

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
  builder_die "Not uploading documentation: must set HELP_KEYMAN_COM in environment."
fi

if [ ! -d "$HELP_KEYMAN_COM/products/" ]; then
  builder_die "HELP_KEYMAN_COM path ($HELP_KEYMAN_COM) does not appear to be valid."
fi

function display_usage {
  echo "Usage: $0 [platform]"
  echo "       $0 --help"
  echo
  echo "  platform should be one of: android, ios, linux, mac, windows, developer."
  echo "  --help               displays this screen and exits"
}

#
# Uploading Keyman documentation
#

function upload {
  local srcpath="$KEYMAN_ROOT/$1"
  local dstpath="$HELP_KEYMAN_COM/$2"

  #
  # Look for help source folder.
  #

  if [[ ! -d "$srcpath" ]]; then
    builder_die "Error: The source path $srcpath does not exist"
    exit 1
  fi

  rm -rf "$dstpath"
  mkdir -p "$dstpath"

  cp -rv "$srcpath"/* "$dstpath/"
}

##
## Upload documentation updates to help.keyman.com
## Paths depend on $platform
##
function upload_keyman_help {
  case $platform in
    android)
      upload android/docs/help products/android/$VERSION_RELEASE
      upload android/docs/engine developer/engine/android/$VERSION_RELEASE
      ;;
    ios)
      upload ios/docs/help products/iphone-and-ipad/$VERSION_RELEASE
      upload ios/docs/engine developer/engine/iphone-and-ipad/$VERSION_RELEASE
      ;;
    linux)
      pushd "$KEYMAN_ROOT/linux/keyman-config" > /dev/null
      ./build.sh build
      popd > /dev/null
      upload linux/docs/help products/linux/$VERSION_RELEASE
      ;;
    mac)
      upload mac/docs/help products/mac/$VERSION_RELEASE
      ;;
    web)
      upload web/docs/engine developer/engine/web/$VERSION_RELEASE
      ;;
    windows)
      # Note: `/windows/src/desktop/help/build.sh web` must be run first
      upload windows/bin/help/md/desktop products/windows/$VERSION_RELEASE
      upload windows/help/engine developer/engine/windows/$VERSION_RELEASE
      ;;
    developer)
      # Note: `/developer/build.sh api` must be run first - covers both uploads
      upload developer/docs/help developer/$VERSION_RELEASE
      upload developer/build/docs developer/$VERSION_RELEASE/reference/api
      upload developer/src/kmc/build/messages developer/$VERSION_RELEASE/reference/messages
      ;;
    *)
      display_usage
    esac
}

#
# Main
#

platform=

# Process command-line arguments
while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    --help|-h)
      display_usage
      exit 0
      ;;
    android | ios | linux | mac | windows | developer | web)
      platform=$key
      ;;
    *)
      echo "$0: invalid option: $key"
      display_usage
      exit 1
  esac
  shift # past the processed argument
done

if [ -z ${platform} ]; then
  display_usage
  exit 1
fi

echo "Uploading Keyman for $platform documentation to help.keyman.com"

upload_keyman_help || exit 1

ci_add_files "$HELP_KEYMAN_COM" .
if ! ci_repo_has_cached_changes "$HELP_KEYMAN_COM"; then
  echo "No changes to commit"
  exit 0
fi

ci_open_pull_request "$HELP_KEYMAN_COM" "auto/$platform-help-$VERSION_WITH_TAG" "auto: Keyman for $platform help deployment"

exit 0