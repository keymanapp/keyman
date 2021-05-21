#!/usr/bin/env bash

# Checkout a matching version of CEF for the
# current release of Keyman. This tag is set in
# /windows/src/CEFVERSION.md

# Terminate script if a command returns an error
set -e

# Terminate script if an unset variable is used
set -u

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
# . "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE

pushd $KEYMAN_CEF4DELPHI_ROOT > /dev/null

CEF_VERSION_MD="$KEYMAN_ROOT/windows/src/CEF_VERSION.md"
CEF_VERSION=`cat $CEF_VERSION_MD | tr -d "[:space:]"`

# Switch to the version of CEF referenced in CEF_VERSION.md
# Note that 14.0 and earlier versions rely on the `master`
# branch, so that is the default branch checked out in the
# build configurations
git switch "v$CEF_VERSION"

# Some files are larger than GitHub's limit of 100MB. For now,
# we can work around this by zipping in the repo and unzipping
# at build time
for zip in *.zip; do
  unzip "$zip"
  rm "$zip"
done

[ -f ./libcef.dll ] || die "File libcef.dll could not be found. Path $KEYMAN_CEF4DELPHI_ROOT may not be valid."

popd > /dev/null

exit 0
