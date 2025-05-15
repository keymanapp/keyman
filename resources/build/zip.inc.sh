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
  EXCLUDE=
  FLAGS=()
  INCLUDE=()
  while [[ $# -gt 0 ]] ; do
    case "$1" in
      -x@*)
        # filename of files to exclude
        EXCLUDE="$1"
        shift
        ;;
      -*)
        # zip/7z flags
        FLAGS+=($1)
        shift
        ;;
      *)
        # files to include in the archive
        INCLUDE+=($1)
        shift
        ;;
    esac
  done

  COMPRESS_CMD=zip
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

      # 7z command
      COMPRESS_CMD="${SEVENZ}"
    fi
  fi

  # Create archive
  builder_echo_debug "${COMPRESS_CMD} ${FLAGS[@]} ${ZIP_FILE} ${INCLUDE[@]} ${EXCLUDE}"
  "${COMPRESS_CMD}" ${FLAGS[@]} ${ZIP_FILE} ${INCLUDE[@]} ${EXCLUDE}

}
