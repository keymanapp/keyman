#!/bin/bash

_shf_base_dir=$(dirname "$BASH_SOURCE")/..

# Designed to determine which set of browsers should be available for local testing,
# based upon the current system OS.
get_builder_OS ( ) {
  # Default value, since it's the most general case/configuration to detect.
  os_id="linux"

  # Subject to change with future improvements.
  if [[ "${OSTYPE}" = "darwin"* ]]; then
    os_id="mac"
  elif [[ "${OSTYPE}" = "msys" ]]; then
    os_id="win"
  elif [[ "${OSTYPE}" = "cygwin" ]]; then
    os_id="win"
  fi
}

# Allows for a quick macOS check for those scripts requiring a macOS environment.
verify_on_mac() {
  if [[ "${OSTYPE}" != "darwin"* ]]; then
    fail "This build script will only run in a Mac environment."
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
    fail "Invalid project specified!"
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
    fail "Invalid platform specified!"
  fi
}

# Gets the folder containing each platform's history.md file, which is also the base folder for most of the platforms.
# Sets $platform_folder accordingly.
get_platform_folder() {
  verify_platform $1

  if [[ $1 = "desktop" || $1 = "developer" ]]; then
    platform_folder="$_shf_base_dir/windows/src/$1"
  elif [[ $1 = "lmlayer" ]]; then
    platform_folder="common/predictive-text"
  else
    platform_folder="$_shf_base_dir/$1"
  fi
}

# The following allows coloring of warning and error lines, but only works if there's a
# terminal attached, so not on the build machine.
if [[ -n "$TERM" ]] && [[ "$TERM" != "dumb" ]]; then
    ERROR_RED=$(tput setaf 1)
    SUCCESS_GREEN=$(tput setaf 2)
    WARNING_YELLOW=$(tput setaf 3)
    NORMAL=$(tput sgr0)
    TERM_HEADING=$(tput setaf 4)
else
    ERROR_RED=
    SUCCESS_GREEN=
    WARNING_YELLOW=
    NORMAL=
    TERM_HEADING=
fi

echo_heading() {
  echo "${TERM_HEADING}$*${NORMAL}"
}

fail() {
    FAILURE_MSG="$1"
    if [[ "$FAILURE_MSG" == "" ]]; then
        FAILURE_MSG="Unknown failure"
    fi
    echo "${ERROR_RED}$FAILURE_MSG${NORMAL}"
    exit 1
}

warn() { echo "${WARNING_YELLOW}$*${NORMAL}"; }

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
        fail "Build failed:  missing $1"
    fi
}

assertDirExists() {
    if ! [ -d $1 ]; then
        fail "Build failed:  missing $1"
    fi
}

assertValidVersionNbr()
{
    if [[ "$1" == "" || ! "$1" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
        fail "Specified version not valid: '$1'. Version should be in the form Major.Minor.BuildCounter"
    fi
}

assertValidPRVersionNbr()
{
    if [[ "$1" == "" || ! "$1" =~ ^[0-9]+\.[0-9]+\.pull\.[0-9]+$ ]]; then
        fail "Specified version not valid: '$1'. Version should be in the form Major.Minor.pull.BuildCounter"
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
    fail "Required -version parameter not specified!"
  fi

  if [ "$KM_TIER" = "" ]; then
    fail "Required -tier parameter not specified!"
  fi

  DOWNLOAD_INFO_FILEPATH="${BASE_PATH}/${BASE_FILE}.download_info"
  if [[ ! -f "${BASE_PATH}/${BASE_FILE}" ]]; then
    fail "Cannot compute file size or MD5 for non-existent DMG file: ${BASE_PATH}/${BASE_FILE}"
  fi

  FILE_EXTENSION="${BASE_FILE##*.}"

  FILE_SIZE=$(stat -f"%z" "${BASE_PATH}/${BASE_FILE}")
  MD5_HASH=$(md5 -q "${BASE_PATH}/${BASE_FILE}")

  if [[ -f "$DOWNLOAD_INFO_FILEPATH" ]]; then
    warn "Overwriting $DOWNLOAD_INFO_FILEPATH"
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
# NOTE: this must be invoked exclusively on the CI system!
#
# Usage:
#
#   set_npm_version VERSION_WITH_TIER
#
set_npm_version () {
  if [ $# == 0 ]; then
    fail "set_npm_version requires a specified version."
  fi
  
  local version=$1
  # We use --no-git-tag-version because our CI system controls version numbering and
  # already tags releases. We also want to have the version of this match the
  # release of Keyman Developer -- these two versions should be in sync. Because this
  # is a large repo with multiple projects and build systems, it's better for us that
  # individual build systems don't take too much ownership of git tagging. :)
  pushd . > /dev/null
  cd "${KEYMAN_ROOT}"
  npm run version "$version"  || fail "Could not set package versions to $version."
  popd > /dev/null
}

# Initializes use of the npm `lerna` package within the repo.
init_lerna() {
  if [ "${KEYMAN_ROOT}" = "" ]; then
    fail "KEYMAN_ROOT not defined; cannot bootstrap repo's dependencies"
  fi

  # At its base, lerna does use an npm-friendly package.json.  Installing that allows the base lerna install to work.
  # We already changed directory in the parent script.
  npm install --no-optional

  # Now that it exists, we can run the following command locally.  (Otherwise, npx will temporarily download everything again, each time!)
  # npm run lerna bootstrap
}

# Accepts an optional parameter.
# #1 - when set to 'false', only ensures that `npm` and `node` are accessible; does not install dependencies.
#
# Designed for use with the projects/packages we have (manually) listed in the base folder's lerna.json.
verify_npm_setup () {
  if [ $# != 0 ]; then
    fetch_deps=$1
  else
    fetch_deps=true
  fi

  # Check if Node.JS/npm is installed.
  type npm >/dev/null ||\
      fail "Build environment setup error detected!  Please ensure Node.js is installed!"

  if [ $fetch_deps = true ]; then
    WORKING_DIRECTORY=`pwd`
    cd "$KEYMAN_ROOT"

    # Ensure the repo's base resources are installed.  (This ensures the local `lerna` install is available.)
    npm install --no-optional

    # Use lerna to ensure repo-internal dependencies are all properly linked 
    # while also installing external dependencies.  Also propagates lerna into
    # each project that can use it (once added as a dev-dependency there)
    #
    # `lerna` will only affect the packages listed the file lerna.json at the repo's base.
    # The "packages" entry therein is MANUALLY managed, not automatic.  Anything not listed
    # therein will not be affected by `lerna` commands.
    #
    # Calls repo's base package 'bootstrap' script, performing the needed `lerna bootstrap -- --no-optional` command.
    get_builder_OS

    # https://github.com/lerna/lerna/issues/789 - there seems to sometimes be a concurrency issue when
    # bootstrapping on macOS.
    if [ ${os_id} == 'mac' ]; then
      # --concurrency=1 is a modification on the lerna command, not for forwarding through the bootstrap command.
      # It's a bit tricky.
      npm run lerna -- bootstrap --concurrency=1 -- --no-optional
    else
      npm run bootstrap
    fi

    # Alternatively, without the specific script:
    #   npx lerna bootstrap -- --no-optional             # ONLY SAFE AT REPO'S BASE (otherwise, temporarily re-downloads `lerna`!)
    #   - or -
    #   npm run lerna -- bootstrap -- -- --no-optional   # Possible to use within sub-packages with proper setup.  (See below.)
    
    # Note:  calling the above WITHOUT changing directories to $KEYMAN_ROOT will re-download lerna.
    #
    # Instead, configuring each package like this:
    #
    # "scripts": {
    #   "lerna": "cd ../ && npm run lerna --"
    # },
    #
    # and calling this:
    #   npm run lerna bootstrap -- -- --no-optional
    # will suffice.
    #
    # (The cd ../ will make the base repo's package.json the default for npm lookups.)
    # (The double '--' is required due to the nested `npm run lerna` call, to ensure the parameters are properly passed through.)

    if [ $? -ne 0 ]; then
      # restore original working directory
      cd "$WORKING_DIRECTORY"
      fail "Build environment setup error detected!  Please ensure Node.js is installed!"
    else
      # restore original working directory
      cd "$WORKING_DIRECTORY"
    fi
  fi
}