#!/usr/bin/env bash

# Allows for a quick macOS check for those scripts requiring a macOS environment.
verify_on_mac() {
  if [[ "${OSTYPE}" != "darwin"* ]]; then
    builder_die "This build script will only run in a Mac environment."
    exit 1
  fi
}

# The list of valid projects that our build scripts ought expect.
projects=("android" "ios" "linux" "lmlayer" "mac" "web" "windows")

# Used to validate a specified 'project' parameter.
verify_project() {
  match=false
  for proj in ${projects[@]}
  do
    if [ $proj = $1 ]; then
      match=true
    fi
  done

  if [ $match = false ]; then
    builder_die "Invalid project specified!"
  fi
}

# The list of valid platforms that our build scripts ought expect.
platforms=("android" "ios" "linux" "lmlayer" "mac" "web" "desktop" "developer")

# Used to validate a specified 'platform' parameter.
verify_platform() {
  match=false
  for proj in ${platforms[@]}
  do
    if [ $proj = $1 ]; then
      match=true
    fi
  done

  if [ $match = false ]; then
    builder_die "Invalid platform specified!"
  fi
}

displayInfo() {
    if [ "$QUIET" != true ]; then
        while [[ $# -gt 0 ]] ; do
            echo $1
            shift # past argument
        done
    fi
}

assertFileExists() {
    if ! [ -f $1 ]; then
        builder_die "Build failed:  missing $1"
    fi
}

assertDirExists() {
    if ! [ -d $1 ]; then
        builder_die "Build failed:  missing $1"
    fi
}

assertValidVersionNbr()
{
    if [[ "$1" == "" || ! "$1" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
        builder_die "Specified version not valid: '$1'. Version should be in the form Major.Minor.BuildCounter"
    fi
}

assertValidPRVersionNbr()
{
    if [[ "$1" == "" || ! "$1" =~ ^[0-9]+\.[0-9]+\.pull\.[0-9]+$ ]]; then
        builder_die "Specified version not valid: '$1'. Version should be in the form Major.Minor.pull.BuildCounter"
    fi
}

dl_info_display_usage() {
    echo "Used to create a metadata file needed on the download site"
    echo "for it to connect to the download.keyman.com API functions."
    echo
    echo "usage: write-download_info <name> <filepath> <version> <tier> <platform>"
    echo
    echo "  name           Specifies the user-friendly name of the product represented by the file."
    echo "  filepath       Specifies the path and file in need of a .download_info metadata file."
    echo "  version        Specifies the build version number, which should be in the"
    echo "                   form Major.Minor.BuildCounter"
    echo "  tier           Specifies tier (typically one of: alpha, beta, stable)."
    echo "  platform       Specifies the target platforms for the file."
    echo "                 (Should be one of: android, ios, mac, web, windows)"
    echo
    echo "The resulting .downloadinfo file will be automatically placed in the same directory"
    echo "as the originally-specified file."
    exit 1
}

write_download_info() {
  #Process file & path information.
  PRODUCT_NAME="$1"
  BASE_PATH="$2"
  KM_VERSION="$3"
  KM_TIER="$4"
  KM_PLATFORM="$5"

  verify_project "$KM_PLATFORM"

  BASE_DIR=$(dirname "${BASE_PATH}")
  BASE_FILE=$(basename "${BASE_PATH}");

  if ([ -h "${BASE_DIR}" ]) then
    while([ -h "${BASE_DIR}" ]) do BASE_PATH=`readlink "${BASE_DIR}"`;
    done
  fi

  assertFileExists "$2"

  pushd . > /dev/null
  cd `dirname ${BASE_DIR}` > /dev/null
  BASE_PATH=`pwd`;
  popd  > /dev/null

  DEST_DIR="$BASE_DIR"

  #Process version parameter.
  assertValidVersionNbr "$3"
  KM_BLD_COUNTER="$((${KM_VERSION##*.}))"

  if [ "$KM_VERSION" = "" ]; then
    builder_die "Required -version parameter not specified!"
  fi

  if [ "$KM_TIER" = "" ]; then
    builder_die "Required -tier parameter not specified!"
  fi

  DOWNLOAD_INFO_FILEPATH="${BASE_PATH}/${BASE_FILE}.download_info"
  if [[ ! -f "${BASE_PATH}/${BASE_FILE}" ]]; then
    builder_die "Cannot compute file size or MD5 for non-existent DMG file: ${BASE_PATH}/${BASE_FILE}"
  fi

  FILE_EXTENSION="${BASE_FILE##*.}"

  FILE_SIZE=$(stat -f"%z" "${BASE_PATH}/${BASE_FILE}")
  MD5_HASH=$(md5 -q "${BASE_PATH}/${BASE_FILE}")

  if [[ -f "$DOWNLOAD_INFO_FILEPATH" ]]; then
    builder_warn "Overwriting $DOWNLOAD_INFO_FILEPATH"
  fi

  echo { > "$DOWNLOAD_INFO_FILEPATH"
  echo "  \"name\": \"${PRODUCT_NAME}\"," >> "$DOWNLOAD_INFO_FILEPATH"
  echo "  \"version\": \"${KM_VERSION}\"," >> "$DOWNLOAD_INFO_FILEPATH"
  echo "  \"date\": \"$(date "+%Y-%m-%d")\"," >> "$DOWNLOAD_INFO_FILEPATH"
  echo "  \"platform\": \"${KM_PLATFORM}\"," >> "$DOWNLOAD_INFO_FILEPATH"
  echo "  \"stability\": \"${KM_TIER}\"," >> "$DOWNLOAD_INFO_FILEPATH"
  echo "  \"file\": \"${BASE_FILE}\"," >> "$DOWNLOAD_INFO_FILEPATH"
  echo "  \"md5\": \"${MD5_HASH}\"," >> "$DOWNLOAD_INFO_FILEPATH"
  echo "  \"type\": \"${FILE_EXTENSION}\"," >> "$DOWNLOAD_INFO_FILEPATH"
  echo "  \"build\": \"${KM_BLD_COUNTER}\"," >> "$DOWNLOAD_INFO_FILEPATH"
  echo "  \"size\": \"${FILE_SIZE}\"" >> "$DOWNLOAD_INFO_FILEPATH"
  echo } >> "$DOWNLOAD_INFO_FILEPATH"
}

# set_version sets the file version on mac/ios projects
set_version ( ) {
  PRODUCT_PATH=$1

  if [ $VERSION ]; then
    if [ $2 ]; then  # $2 = product name.
      echo "Setting version numbers in $2 to $VERSION."
    fi
    /usr/libexec/Plistbuddy -c "Set CFBundleVersion $VERSION" "$PRODUCT_PATH/Info.plist"
    /usr/libexec/Plistbuddy -c "Set CFBundleShortVersionString $VERSION" "$PRODUCT_PATH/Info.plist"
  fi
}


# Uses npm to set the current package version (package.json).
#
# This sets the version according to the current VERSION_WITH_TAG.
#
# Usage:
#
#   set_npm_version
#
set_npm_version () {
  # We use --no-git-tag-version because our CI system controls version numbering and
  # already tags releases. We also want to have the version of this match the
  # release of Keyman Developer -- these two versions should be in sync. Because this
  # is a large repo with multiple projects and build systems, it's better for us that
  # individual build systems don't take too much ownership of git tagging. :)
  npm version --allow-same-version --no-git-tag-version --no-commit-hooks "$VERSION_WITH_TAG"
}

#
# Verifies that node is installed, and installs npm packages, but only once per
# build invocation
#
verify_npm_setup() {
  # We'll piggy-back on the builder module dependency build state to determine
  # if npm ci has been called in the current script invocation. Adding the
  # prefix /external/ to module name in order to differentiate between this and
  # internal modules (although it is unlikely to ever collide!); we will also
  # use this pattern for other similar external dependencies in future. These
  # functions are safe to call even in a non-builder context (they do nothing or
  # return 1 -- not built)
  if builder_has_module_been_built /external/npm-ci; then
    return 0
  fi
  builder_set_module_has_been_built /external/npm-ci

  # Check if Node.JS/npm is installed.
  type npm >/dev/null ||\
    builder_die "Build environment setup error detected!  Please ensure Node.js is installed!"

  pushd "$KEYMAN_ROOT" > /dev/null
  npm ci
  popd > /dev/null
}
