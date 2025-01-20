#!/usr/bin/env bash
# shellcheck disable=SC2154 # (variables are set in build-utils.sh)

output_log() {
  echo "$1" >&2
  builder_echo "$1"
}

output_ok() {
  echo ":heavy_check_mark: $1" >&2
  builder_echo green "OK: $1"
}

output_warning() {
  echo ":warning: $1" >&2
  builder_echo warning "WARNING: $1"
}

output_error() {
  echo ":x: $1" >&2
  builder_echo error "ERROR: $1"
}

check_api_not_changed() {
  if [[ -z "${BIN_PKG:-}" ]]; then
    output_warning "Skipping check for API change because binary Debian package not specified"
    return
  fi
  # Checks that the API did not change compared to what's documented in the .symbols file
  tmpDir=$(mktemp -d)
  # shellcheck disable=SC2064
  trap "rm -rf \"${tmpDir}\"" ERR
  dpkg -x "${BIN_PKG}" "${tmpDir}"
  mkdir -p debian/tmp/DEBIAN
  if dpkg-gensymbols -v"${VERSION}" -p"${PKG_NAME}" -e"${tmpDir}"/usr/lib/x86_64-linux-gnu/"${LIB_NAME}".so* -c4; then
    output_ok "${LIB_NAME} API didn't change"
  else
    output_error "${LIB_NAME} API changed"
    EXIT_CODE=4
  fi
  cd "${REPO_ROOT}/linux"
  rm -rf "${tmpDir}"
  trap ERR
}

#
# Compare the SHA of the base and head commits for changes to the .symbols file
#
is_symbols_file_changed() {
  local CHANGED_REF CHANGED_BASE
  CHANGED_REF=$(git rev-parse "${GIT_SHA}":"linux/debian/${PKG_NAME}.symbols")
  CHANGED_BASE=$(git rev-parse "${GIT_BASE}":"linux/debian/${PKG_NAME}.symbols")
  if [[ "${CHANGED_REF}" == "${CHANGED_BASE}" ]]; then
    return 1
  fi
  return 0
}

get_changes() {
  local WHAT_CHANGED
  WHAT_CHANGED=$(git diff -I "^${LIB_NAME}.so" "$1".."$2" | diffstat -m -t | grep "${PKG_NAME}.symbols" )

  IFS=',' read -r -a CHANGES <<< "${WHAT_CHANGED:-0,0,0}"
}

check_updated_version_number() {
  # Checks that the package version number got updated in the .symbols file if it got changed
  # shellcheck disable=SC2310
  if is_symbols_file_changed; then
    # .symbols file changed, now check if the package version got updated as well
    # Note: We don't check that ALL changes in that file have an updated package version -
    # we hope this gets flagged in code review.
    # Note: This version number check may not match the actual released version, if the branch
    # is out of date when it is merged to the release branch (master/beta/stable-x.y). If this
    # is considered important, then make sure the branch is up to date, and wait for test
    # builds to complete, before merging.
    get_changes "${GIT_BASE}" "${GIT_SHA}"
    INSERTED="${CHANGES[0]}"
    DELETED="${CHANGES[1]}"
    MODIFIED="${CHANGES[2]}"

    if (( DELETED > 0 )) && (( MODIFIED == 0 )) && (( INSERTED == 0)); then
        # If only lines got removed we basically skip this test. A later check will
        # test that the API version got updated.
        output_ok "${PKG_NAME}.symbols file did change but only removed lines"
    else
      local version_base version_head
      version_head=$(get_highest_version_in_symbols_file "${GIT_SHA}")
      version_base=$(get_highest_version_in_symbols_file "${GIT_BASE}")
      if (( $(compare_versions "${version_head}" "${version_base}") > 0 )); then
        output_ok "${PKG_NAME}.symbols file got updated with package version number"
      else
        output_error "${PKG_NAME}.symbols file got changed without changing the package version number of the symbol"
        EXIT_CODE=1
      fi
    fi
  else
    output_ok "${PKG_NAME}.symbols file didn't change"
  fi
}

compare_versions() {
  local first_parts second_parts
  IFS='.' read -r -a first_parts <<< "$1"
  IFS='.' read -r -a second_parts <<< "$2"
  if (( first_parts[0] < second_parts[0] )); then
    echo -1
  elif (( first_parts[0] > second_parts[0] )); then
    echo 1
  elif (( first_parts[1] < second_parts[1] )); then
    echo -1
  elif (( first_parts[1] > second_parts[1] )); then
    echo 1
  elif (( first_parts[2] < second_parts[2] )); then
    echo -1
  elif (( first_parts[2] > second_parts[2] )); then
    echo 1
  else
    echo 0
  fi
}

get_highest_version_in_symbols_file() {
  local sha="$1"
  local symbol_lines line line_version
  local max_version=0

  tmpfile=$(mktemp)
  if ! git cat-file blob "${sha}:linux/debian/${PKG_NAME}.symbols" > "${tmpfile}" 2>/dev/null; then
    rm "${tmpfile}"
    return 1
  fi

  # Start with fourth line which is where symbols start
  mapfile -s 3 symbol_lines < "${tmpfile}"
  for line in "${symbol_lines[@]}"; do
    #  km_core_actions_dispose@Base 17.0.197
    line_version=${line##* }
    if (( $(compare_versions "${line_version}" "${max_version}") > 0 )); then
      max_version="${line_version}"
    fi
  done

  echo "${max_version}"

  rm "${tmpfile}"
  return 0
}

get_api_version_in_symbols_file() {
  # Retrieve symbols file at commit $1 and extract "1" from
  # "libkeymancore.so.1 libkeymancore1 #MINVER#"
  local firstline tmpfile
  local sha="$1"

  tmpfile=$(mktemp)
  if ! git cat-file blob "${sha}:linux/debian/${PKG_NAME}.symbols" > "${tmpfile}" 2>/dev/null; then
    rm "${tmpfile}"
    echo "-1"
    return
  fi

  firstline="$(head -1 "${tmpfile}")"
  firstline="${firstline#"${LIB_NAME}".so.}"
  firstline="${firstline%% *}"

  rm "${tmpfile}"
  echo "${firstline}"
}

get_api_version_from_core() {
  # Retrieve CORE_API_VERSION.md from commit $1 and extract major version
  # number ("1") from "1.0.0"
  local api_version tmpfile
  local sha="$1"
  tmpfile=$(mktemp)

  if ! git cat-file blob "${sha}:core/CORE_API_VERSION.md" > "${tmpfile}" 2>/dev/null; then
    rm "${tmpfile}"
    echo "-1"
    return
  fi

  api_version=$(cat "${tmpfile}")
  api_version=${api_version%%.*}

  rm "${tmpfile}"
  echo "${api_version}"
}

# Check if the API version got updated
# Returns:
#   0 - if the API version got updated
#   1 - the .symbols file got changed but the API version didn't get updated
#   2 - if we're in the alpha tier and the API version got updated since
#       the last stable version
# NOTE: it is up to the caller to check if this is a major version
# change that requires an API version update.
# Check if the API version got updated
# Returns:
#   0 - if the API version got updated
#   1 - the .symbols file got changed but the API version didn't get updated
#   2 - if we're in the alpha tier and the API version got updated since
#       the last stable version
# NOTE: it is up to the caller to check if this is a major version
# change that requires an API version update.
is_api_version_updated() {
  local OLD_API_VERSION NEW_API_VERSION TIER
  OLD_API_VERSION=$(get_api_version_in_symbols_file "${GIT_BASE}")
  NEW_API_VERSION=$(get_api_version_in_symbols_file "${GIT_SHA}")
  if (( NEW_API_VERSION > OLD_API_VERSION )); then
    echo "0"
    return
  fi

  # API version didn't change. However, that might be ok if we're in alpha
  # and a major change happened previously.
  TIER=$(cat "${REPO_ROOT}/TIER.md")
  case ${TIER} in
    alpha)
      local STABLE_VERSION STABLE_API_VERSION STABLE_BRANCH
      STABLE_VERSION=$((${VERSION%%.*} - 1))
      STABLE_BRANCH="origin/stable-${STABLE_VERSION}.0"
      STABLE_API_VERSION=$(get_api_version_in_symbols_file "${STABLE_BRANCH}")
      if (( STABLE_API_VERSION == -1 )); then
        # .symbols file doesn't exist in stable branch, so let's check CORE_API_VERSION.md. That
        # doesn't exist in 16.0 but appeared in 17.0.
        STABLE_API_VERSION=$(get_api_version_from_core "${STABLE_BRANCH}")
        if (( STABLE_API_VERSION == -1 )); then
          # CORE_API_VERSION.md doesn't exist either
          if (( NEW_API_VERSION > 0 )); then
            # .symbols and CORE_API_VERSION.md file don't exist in stable branch; however, we
            # incremented the version number compared to 16.0, so that's ok
            echo "2"
            return
          fi
        fi
      fi
      if (( NEW_API_VERSION > STABLE_API_VERSION )); then
        echo "2"
        return
      fi ;;
    *)
      ;;
  esac

  echo "1"
}

check_for_major_api_changes() {
  # Checks that API version number gets updated if API changes
  local WHAT_CHANGED CHANGES INSERTED DELETED MODIFIED UPDATED

  # shellcheck disable=2310
  if ! is_symbols_file_changed; then
    output_ok "No major API change"
    return
  fi

  get_changes "${GIT_BASE}" "${GIT_SHA}"
  INSERTED="${CHANGES[0]}"
  DELETED="${CHANGES[1]}"
  MODIFIED="${CHANGES[2]}"

  if (( DELETED > 0 )) || (( MODIFIED > 0 )); then
    builder_echo "Major API change: ${DELETED} lines deleted and ${MODIFIED} lines modified"
    UPDATED=$(is_api_version_updated)
    if [[ ${UPDATED} == 1 ]]; then
      output_error "Major API change without updating API version number in ${PKG_NAME}.symbols file"
      EXIT_CODE=2
    elif [[ ${UPDATED} == 2 ]]; then
      output_ok "API version number got previously updated in ${PKG_NAME}.symbols file after major API change; no change within alpha necessary"
    else
      output_ok "API version number got updated in ${PKG_NAME}.symbols file after major API change"
    fi
  elif (( INSERTED > 0 )); then
    output_ok "Minor API change: ${INSERTED} lines added"
    # We currently don't check version number for minor API changes
  else
    output_ok "No major API change"
  fi
}

check_for_api_version_consistency() {
  # Checks that the (major) API version number in the .symbols file and
  # in CORE_API_VERSION.md are the same
  local symbols_version api_version
  symbols_version=$(get_api_version_in_symbols_file "HEAD")
  api_version=$(get_api_version_from_core "HEAD")

  if (( symbols_version == api_version )); then
    output_ok "API version in .symbols file and in CORE_API_VERSION.md is the same"
  else
    output_error "API version in .symbols file and in CORE_API_VERSION.md is different"
    EXIT_CODE=3
  fi
}

verify_api_action() {
  local SONAME
  SONAME=$(get_api_version_from_core "HEAD")
  LIB_NAME=libkeymancore
  PKG_NAME="${LIB_NAME}${SONAME}"
  if [[ ! -f debian/${PKG_NAME}.symbols ]]; then
    output_error "Missing ${PKG_NAME}.symbols file"
    exit 0
  fi

  EXIT_CODE=0
  check_api_not_changed
  check_updated_version_number
  check_for_major_api_changes
  check_for_api_version_consistency
  exit "${EXIT_CODE}"
}
