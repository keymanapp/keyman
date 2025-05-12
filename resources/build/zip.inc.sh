#!/usr/bin/env bash
#
# This script contains zip and unzip utilities to function for all environments.
# unzip is available in all environments
# zip is not available by default on Windows, but can be manually installed.
# If zip is not available, then fall back to 7z for zipping.
#

# convert parameters into array
# $1 - count of parameters
# $@ - remaining parameters to convert into array
# returns array of strings
function _parse_args() {
  count=$1
  shift
  value=()
  for (( i=0; i<$count; i++)); do
    value+=($1)
    shift
  done
  echo ${value[@]}
}

# [--name zip filename]
# [--flags   n  list of n flags to pass to zip command]
# [--include n  list of n files to include in zip]
# [--exclude n  list of n files to exclude from zip]
function zip_files() {

  # Parse parameters
  FLAGS=()
  INCLUDE=()
  EXCLUDE=()
  while [[ $# -gt 0 ]] ; do
    case "$1" in
      --name|-n)
        local ZIP_FILE="$2"
        shift 2
        echo "ZIP_FILE: ${ZIP_FILE}"
        ;;
      --flags|-f)
        count="$2"
        shift 2
        FLAGS=$(_parse_args $count $@)
        shift $count
        echo "FLAGS: ${FLAGS[@]}"
        ;;

      --include|-i)
        count="$2"
        shift 2
        INCLUDE=$(_parse_args $count $@)
        shift $count
        echo "INCLUDE: ${INCLUDE[@]}"
        ;;

      --exclude|-e)
        count="$2"
        shift 2
        EXCLUDE=$(_parse_args $count $@)
        shift $count
        echo "EXCLUDE: ${EXCLUDE[@]}"
        break
        ;;

      *)
        echo "Error: Unexpected argument \"$1\". Exiting."
        exit 1
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

    for ignoreFile in "${EXCLUDE[@]}"
    do
      IGNORE+=" -x \\*\\*/${ignoreFile}"
    done
    # zip command, excluding build.sh files
    echo ${COMPRESS_CMD} ${FLAGS[@]} ${ZIP_FILE} ${INCLUDE[@]} ${IGNORE[@]}
    ${COMPRESS_CMD} ${FLAGS[@]} ${ZIP_FILE} ${INCLUDE[@]} ${IGNORE[@]}
  fi

}

