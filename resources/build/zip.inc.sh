# shellscript shell=bash
#
# This script contains zip and unzip (TODO) utilities to function for all environments.
# unzip is available in all environments
# zip and 7z are not available by default on Windows, but can be manually installed.
# If 7z is not available, then fall back to zip for zipping.
# 7z on Windows requires env SEVENZ_HOME.
#
# TODO: refactor with /resources/build/win/zip.inc.sh

# Add files to create a zip/7z archive with the following parameters (in order)
# [zip filename]
# [list of flags to pass to zip command] Flags start with a single-dash
#   -x@filename for a file containing list of files to exclude from the archive
#   -xr!name    to exclude files matching name from the archive
#   -* all other flags
#   Flags passed in are treated as zip parameters, and internally converterted
#   to 7z flags as applicable
# [list of files to include in zip]
function add_zip_files() {

  # Parse parameters

  # $1 for zip filename
  local ZIP_FILE="$1"
  shift

  # Parse rest of parameters
  local ZIP_FLAGS=()
  local SEVENZ_FLAGS=('a') # 7z requires a command
  local INCLUDE=()
  local EXCLUDE_FILE
  while [[ $# -gt 0 ]] ; do
    case "$1" in
      -r)
        # recursive paths - Identical flag to zip and 7z
        ZIP_FLAGS+=($1)
        SEVENZ_FLAGS+=($1)
        shift
        ;;
      -x@*)
        # Filename for a file containing list of files to exclude from the archive - Identical flag to zip and 7z
        ZIP_FLAGS+=($1)
        SEVENZ_FLAGS+=($1)
        shift
        ;;
      -xr!*)
        # Recursively exclude the file after -xr! from the archive
        EXCLUDE_FILE=$(mktemp)
        find . -name ${1##-xr!} > "${EXCLUDE_FILE}"
        ZIP_FLAGS+=("-x@${EXCLUDE_FILE}")
        SEVENZ_FLAGS+=($1)
        shift
        ;;

      # Zip flags that have a corresponding 7z flag
      -q)
        # quiet mode  -> disable progress indicator, set output log level 0
        ZIP_FLAGS+=($1)
        SEVENZ_FLAGS+=("-bd")
        SEVENZ_FLAGS+=("-bb0")
        shift
        ;;
      -[0123456789])
        # Compression level where
        # -0 indicates no compression
        # -1 indicates low compression (fastest)
        # -9 indicates ultra compression (slowest)
        ZIP_FLAGS+=($1)
        if [[ $1 =~ -([0-9]) ]]; then
          SEVENZ_FLAGS+=("-mx${BASH_REMATCH[1]}")
        fi
        shift;
        ;;

      -*)
        # Remaining zip flags that don't apply to 7z
        ZIP_FLAGS+=($1)
        shift
        ;;

      *)
        # files to include in the archive
        INCLUDE+=($1)
        shift
        ;;
    esac
  done

  _verify_input "${INCLUDE[@]}"

  local COMPRESS_CMD=zip
  if [[ ! -z "${SEVENZ_HOME:-}" ]] || command -v 7z > /dev/null 2>&1; then
    # Use 7z if available
    if [[ -z "${SEVENZ+x}" ]]; then
      case "${OSTYPE}" in
        "cygwin"|"msys")
          SEVENZ="${SEVENZ_HOME}"/7z.exe
          ;;
        *)
          SEVENZ=7z
          ;;
      esac
    fi

    # 7z command to add files so clear zip flags
    COMPRESS_CMD="${SEVENZ}"
    ZIP_FLAGS=()
  else
    # Assume zip is available, so clear 7z flags
    COMPRESS_CMD=zip
    SEVENZ_FLAGS=()
  fi

  # Create archive
  # builder_echo_debug "${COMPRESS_CMD} ${SEVENZ_FLAGS[@]} ${ZIP_FLAGS[@]} ${ZIP_FILE} ${INCLUDE[@]}"
  "${COMPRESS_CMD}" ${SEVENZ_FLAGS[@]} ${ZIP_FLAGS[@]} ${ZIP_FILE} ${INCLUDE[@]}

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
