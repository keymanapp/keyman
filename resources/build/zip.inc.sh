#!/usr/bin/env bash
#
# This script contains zip and unzip utilities to function for all environments.
# unzip is available in all environments
# zip is not available by default on Windows, but can be manually installed.
# If zip is not available, then fall back to 7z for zipping.
#

# * `flags`        flags for the zip command
# * `zip file`     zip file to create
# * `files`        files to include in the zip file
function zip_files() {
  local FLAGS="$1"
  shift 1
  local ZIP_FILE="$1"
  shift 1
  local FILE_LIST="$@"

  COMPRESS_CMD=zip
  if ! command -v zip 2>&1 > /dev/null; then
    # Fallback to 7z
    if [[ -z "${SEVENZ+x}" ]]; then
      case "${OSTYPE}" in
        "cygwin")
          SEVENZ="${SEVENZ_HOME}"/7z.exe
          ;;
        "msys")
          SEVENZ="${SEVENZ_HOME}"/7z.exe
          ;;
        *)
          SEVENZ=7z
          ;;
      esac

      # 7z command, excluding build.sh files
      "${SEVENZ}" ${FLAGS} ${ZIP_FILE} ${FILE_LIST} '-xr!build.sh'
    fi
  else  
    # zip command, excluding build.sh files
    ${COMPRESS_CMD} ${FLAGS} ${ZIP_FILE} ${FILE_LIST} -x \*\*/build\.sh
  fi

}

