#!/usr/bin/env bash
#
# This script contains zip and unzip utilities to function for all environments.
# unzip is available in all environments
# zip is not available by default on Windows, but can be manually installed.
# If zip is not available, then fall back to 7z for zipping.
#
# TODO: refactor with /resources/build/win/zip.inc.sh

# zip/7z to create an archive with the following parameters (in order)
# [zip filename]
# [filename of files to exclude in the zip]
# [list of flags to pass to zip command, 'a' or leading with single-dash]
# [list of files to include in zip]
function zip_files() {

  # Parse parameters

  # $1 for zip filename
  local ZIP_FILE="$1"
  shift

  # $2 for filename containing files to exclude
  EXCLUDE=$1
  shift
  pwd
  if ! test -f ${EXCLUDE}; then
    builder_die "Filename of files to exclude '${EXCLUDE}' does not exist"
  fi

  # Parse parameters
  FLAGS=()
  INCLUDE=()
  while [[ $# -gt 0 ]] ; do
    case "$1" in
      a|-*)
        FLAGS+=($1)
        shift
        ;;

      *)
        INCLUDE+=($1)
        shift
        ;;
    esac
  done

  COMPRESS_CMD=zip
  IGNORE=()
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

      for ignoreFile in "${EXCLUDE[@]}"
      do
        IGNORE+=" -xr!${ignoreFile}"
      done
      # 7z command, excluding build.sh files
      "${SEVENZ}" ${FLAGS[@]} ${ZIP_FILE} ${INCLUDE[@]} ${IGNORE[@]}
    fi
  else

    # zip command
    builder_echo_debug "${COMPRESS_CMD} ${FLAGS[@]} ${ZIP_FILE} ${INCLUDE[@]} -x@${EXCLUDE}"
    ${COMPRESS_CMD} ${FLAGS[@]} ${ZIP_FILE} ${INCLUDE[@]} -x@${EXCLUDE}
  fi

}

