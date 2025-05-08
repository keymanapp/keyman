#!/usr/bin/env bash
#
# This script contains zip and unzip utilities to function for all environments.
# unzip is available in all environments
# zip is not available by default on Windows, but can be manually installed.
# If zip is not available, then fall back to 7z for zipping.
#

# Parameters
# * `zip file`     zip file to create
# * `flags`        array of flags for the zip command
# * `include`      array of files to include in the zip file
# * `exclude`      array files to exclude in the zip file
function zip_files() {
  local -n _ZIP_FILE=$1
  local -n _FLAGS=$2
  local -n _INCLUDE=$3
  local -n _EXCLUDE=$4

  local _exclude_str=""

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

      for i in "$_EXCLUDE[@]}"
      do
        _exclude_str+=" -xr!${i}"
      done

      echo "sevenz command: ${SEVENZ} ${_FLAGS} ${_ZIP_FILE} ${_INCLUDE} ${_exclude_str}"
      # 7z command, excluding build.sh files
      "${SEVENZ}" ${_FLAGS} ${_ZIP_FILE} ${_INCLUDE} ${_exclude_str}
    fi
  else

    for i in "$_EXCLUDE[@]}"
    do
      _exclude_str+=" -x \*\*/${i}"
    done

    echo "zip command"
    # zip command, excluding build.sh files
    ${COMPRESS_CMD} ${_FLAGS} ${_ZIP_FILE} ${_INCLUDE} ${_exclude_str}
  fi

}

