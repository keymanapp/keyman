# shellcheck shell=bash
#
# This script contains zip and unzip (TODO) utilities to function for all environments.
# unzip is available in all environments
# zip is not available by default on Windows, but can be manually installed.
# If zip is not available, then fall back to 7z for zipping.
# 7z requires env SEVENZ_HOME.
#
# The script considers the GO_FAST env variable:
# - if set to 1 we use a lower compression level, resulting in faster builds
#   and bigger artifacts
# - if set to 0 we use a higher compression level, resulting in slower builds
#   and smaller artifacts
# - if not set we rely on the default compression level

# Add files to create a zip/7z archive
# Parameters:
# - [zip filename]
# - [list of files to include in zip]
#
# Optional flags:
#   -x@filename for a file containing list of files to exclude from the archive
#   -xr!name    to exclude files matching name from the archive
#   -r          to recursively include files in subdirectories
#   -q          to enable quiet mode (zip) or disable progress indicator
#               and set output log level 0 (7z)
#   -0 to -9    for compression level with -0 indicating no compression,
#               -1 indicating low compression (fastest) and -9 indicating
#               ultra compression (slowest)
#   -* all other flags
# Flags start with a single-dash and get passed to zip command. They can come
# before, after or in between the zip filename and the files to include.
# Flags passed in are treated as zip parameters, and internally converterted
# to 7z flags as applicable

function add_zip_files() {

  # Parse parameters
  local ZIP_FILE
  local ZIP_FLAGS=()
  local SEVENZ_FLAGS=('a') # 7z requires a command
  local INCLUDE=()
  local EXCLUDE_FILE
  local HAS_COMPRESSION=false
  while [[ $# -gt 0 ]] ; do
    case "$1" in
      -r)
        # recursive paths - Identical flag to zip and 7z
        ZIP_FLAGS+=("$1")
        SEVENZ_FLAGS+=("$1")
        shift
        ;;
      -x@*)
        # Filename for a file containing list of files to exclude from the archive - Identical flag to zip and 7z
        ZIP_FLAGS+=("$1")
        SEVENZ_FLAGS+=("$1")
        shift
        ;;
      -xr!*)
        # Recursively exclude the file after -xr! from the archive
        EXCLUDE_FILE=$(mktemp)
        find . -name "${1##-xr!}" > "${EXCLUDE_FILE}"
        ZIP_FLAGS+=("-x@${EXCLUDE_FILE}")
        SEVENZ_FLAGS+=("$1")
        shift
        ;;

      # Zip flags that have a corresponding 7z flag
      -q)
        # quiet mode  -> disable progress indicator, set output log level 0
        ZIP_FLAGS+=("$1")
        SEVENZ_FLAGS+=("-bd")
        SEVENZ_FLAGS+=("-bb0")
        shift
        ;;
      -[0123456789])
        # Compression level where
        # -0 indicates no compression
        # -1 indicates low compression (fastest)
        # -9 indicates ultra compression (slowest)
        HAS_COMPRESSION=true
        ZIP_FLAGS+=("$1")
        if [[ $1 =~ -([0-9]) ]]; then
          SEVENZ_FLAGS+=("-mx${BASH_REMATCH[1]}")
        fi
        shift;
        ;;

      -*)
        # Remaining zip flags that don't apply to 7z
        ZIP_FLAGS+=("$1")
        shift
        ;;

      *)
        # files to include in the archive. First non-flag parameter is the zip file name.
        if [[ -z "${ZIP_FILE:-}" ]]; then
          ZIP_FILE="$1"
        else
          INCLUDE+=("$1")
        fi
        shift
        ;;
    esac
  done

  # If GO_FAST is set, we use either fast or slow compression level.
  # Otherwise we use the defaults.
  if ! ${HAS_COMPRESSION}; then
    if [[ "${GO_FAST:-}" == "1" ]]; then
      SEVENZ_FLAGS+=("-mx1")
      ZIP_FLAGS+=("-1")
    elif [[ "${GO_FAST:-}" == "0" ]]; then
      SEVENZ_FLAGS+=("-mx9")
      ZIP_FLAGS+=("-9")
    fi
  fi

  _verify_input "${INCLUDE[@]}"

  local COMPRESS_CMD=zip
  if ! command -v zip > /dev/null 2>&1; then
    # Fallback to 7z
    if [[ -z "${SEVENZ+x}" ]]; then
      if builder_is_windows; then
        if [[ -z "${SEVENZ_HOME+x}" ]]; then
          SEVENZ="$(command -v 7z.exe)"
        else
          SEVENZ="${SEVENZ_HOME}/7z.exe"
        fi
      else
        SEVENZ=7z
      fi
    fi

    # 7z command to add files so clear zip flags
    COMPRESS_CMD="${SEVENZ}"
    ZIP_FLAGS=()
  else
    # Using zip so clear 7z flags
    SEVENZ_FLAGS=()
  fi

  # Create archive
  # builder_echo_debug "${COMPRESS_CMD} ${SEVENZ_FLAGS[@]} ${ZIP_FLAGS[@]} ${ZIP_FILE} ${INCLUDE[@]}"
  "${COMPRESS_CMD}" "${SEVENZ_FLAGS[@]}" "${ZIP_FLAGS[@]}" "${ZIP_FILE}" "${INCLUDE[@]}"

  if [[ -n "${EXCLUDE_FILE:-}" ]]; then
    rm "${EXCLUDE_FILE}"
  fi
}

_verify_input() {
  local curdir file absfile
  curdir="$(pwd)"

  for file in "$@"; do
    absfile=$(readlink --canonicalize-missing "${file}")
    if [[ "${absfile}" != "${curdir}"* ]]; then
      # 7z and zip behave differently if adding files that are not in the current
      # directory. For files with relative paths, zip will add them relative to
      # the current directory which is not what we want and can cause
      # problems for the user. Therefore we disallow files that are not in the
      # current directory.
      builder_die "File ${file} is not in the current directory"
    fi
  done
}
