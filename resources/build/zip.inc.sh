#!/usr/bin/env bash
#
# This script contains zip and unzip (TODO) utilities to function for all environments.
# unzip is available in all environments
# zip is not available by default on Windows, but can be manually installed.
# If zip is not available, then fall back to 7z for zipping.
# 7z requires env SEVENZ_HOME.
#
# TODO: refactor with /resources/build/win/zip.inc.sh

# zip/7z to create an archive with the following parameters (in order)
# [zip filename]
# [list of flags to pass to zip command. Flags start with a single-dash
#   -x@filename for a file containing list of files to exclude from the archive
#   -* all other flags]
# [list of files to include in zip]
function zip_files() {

  # Parse parameters

  # $1 for zip filename
  local ZIP_FILE="$1"
  shift

  # Parse rest of parameters
  local ZIP_FLAGS=()
  local SEVENZ_FLAGS=('a') # 7z requires a command
  local INCLUDE=()
  while [[ $# -gt 0 ]] ; do
    case "$1" in
      -r)
        # Common flags to zip and 7z
        ZIP_FLAGS+=($1)
        SEVENZ_FLAGS+=($1)
        shift
        ;;
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
        if [[ $1 =~ -([0-9]) ]] then
          SEVENZ_FLAGS+=("-mx${BASH_REMATCH[1]}")
        fi  
        shift;
        ;;
      -*)
        # Rest of zip flags.
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

  local COMPRESS_CMD=zip
  if ! command -v zip 2>&1 > /dev/null; then
    # Fallback to 7z
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
    # Using zip so clear 7z flags
    SEVENZ_FLAGS=()
  fi

  # Create archive
  builder_echo_debug "${COMPRESS_CMD} ${SEVENZ_FLAGS[@]} ${ZIP_FLAGS[@]} ${ZIP_FILE} ${INCLUDE[@]}"
  "${COMPRESS_CMD}" ${SEVENZ_FLAGS[@]} ${ZIP_FLAGS[@]} ${ZIP_FILE} ${INCLUDE[@]}

}
