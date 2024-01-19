#!/usr/bin/env bash
# shellcheck disable=SC2154 # (variables are set in build-utils.sh)
# Actions for creating a Debian source package. Used by deb-packaging.yml GHA.

set -eu
shopt -s inherit_errexit

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe \
  "Helper for building Debian packages." \
  "dependencies               Install dependencies as found in debian/control" \
  "source+                    Build source package" \
  "verify                     Verify API" \
  "--gha                      Build from GitHub Action" \
  "--bin-pkg=BIN_PKG          Path and name of binary Debian package (for verify action)" \
  "--git-sha=GIT_SHA          The SHA of the HEAD commit, e.g. of the PR branch (for verify action)" \
  "--git-base=GIT_BASE        The ref of the base commit, e.g. usually the name of the base" \
  "                           branch, e.g. master, stable-16.0 (for verify action)"

builder_parse "$@"

cd "${REPO_ROOT}/linux"

if builder_has_option --gha; then
  START_STEP="::group::${COLOR_GREEN}"
  END_STEP="::endgroup::"
else
  START_STEP="${COLOR_GREEN}"
  END_STEP=""
fi

dependencies_action() {
  sudo mk-build-deps --install --tool='apt-get -o Debug::pkgProblemResolver=yes --no-install-recommends --yes' debian/control
}

source_action() {
  echo "${START_STEP}Make source package for keyman${COLOR_RESET}"

  echo "${START_STEP}reconfigure${COLOR_RESET}"
  ./scripts/reconf.sh
  echo "${END_STEP}"

  echo "${START_STEP}Make origdist${COLOR_RESET}"
  ./scripts/dist.sh origdist
  echo "${END_STEP}"

  echo "${START_STEP}Make deb source${COLOR_RESET}"
  ./scripts/deb.sh sourcepackage
  echo "${END_STEP}"

  mv builddebs/* "${OUTPUT_PATH:-..}"
}

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
  # Checks that the API did not change compared to what's documented in the .symbols file
  tmpDir=$(mktemp -d)
  # shellcheck disable=SC2064
  trap "rm -rf \"${tmpDir}\"" ERR
  dpkg -x "${BIN_PKG}" "${tmpDir}"
  mkdir -p debian/tmp/DEBIAN
  dpkg-gensymbols -v"${VERSION}" -p"${PKG_NAME}" -e"${tmpDir}"/usr/lib/x86_64-linux-gnu/"${LIB_NAME}".so* -c4
  output_ok "${LIB_NAME} API didn't change"
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
    if ! git log -p -1 -- "debian/${PKG_NAME}.symbols" | grep -q "${VERSION}"; then
      output_error "${PKG_NAME}.symbols file got changed without changing the package version number of the symbol"
      EXIT_CODE=1
    else
      output_ok "${PKG_NAME}.symbols file got updated with package version number"
    fi
  else
    output_ok "${PKG_NAME}.symbols file didn't change"
  fi
}

get_api_version_in_symbols_file() {
  # Extract 1 from "libkeymancore.so.1 libkeymancore #MINVER#"
  local firstline
  firstline="$(head -1 "debian/${PKG_NAME}.symbols")"
  firstline="${firstline#"${PKG_NAME}".so.}"
  firstline="${firstline%% *}"
  echo "${firstline}"
}

is_api_version_updated() {
  local NEW_VERSION OLD_VERSION
  git checkout "${GIT_BASE}" -- "debian/${PKG_NAME}.symbols"
  OLD_VERSION=$(get_api_version_in_symbols_file)
  git checkout "${GIT_SHA}" -- "debian/${PKG_NAME}.symbols"
  NEW_VERSION=$(get_api_version_in_symbols_file)
  if (( NEW_VERSION > OLD_VERSION )); then
    return 0
  fi
  return 1
}

check_for_major_api_changes() {
  # Checks that API version number gets updated if API changes
  local WHAT_CHANGED CHANGES INSERTED DELETED MODIFIED

  # shellcheck disable=2310
  if ! is_symbols_file_changed; then
    output_ok "No major API change"
    return
  fi

  WHAT_CHANGED=$(git diff "${GIT_BASE}".."${GIT_SHA}" -- "debian/${PKG_NAME}.symbols" | diffstat -m -t | tail -1)

  IFS=',' read -r -a CHANGES <<< "${WHAT_CHANGED}"
  INSERTED="${CHANGES[0]}"
  DELETED="${CHANGES[1]}"
  MODIFIED="${CHANGES[2]}"

  if (( DELETED > 0 )) || (( MODIFIED > 0 )); then
    builder_echo "Major API change: ${DELETED} lines deleted and ${MODIFIED} lines modified"
    # shellcheck disable=2310
    if ! is_api_version_updated; then
      output_error "Major API change without updating API version number in ${PKG_NAME}.symbols file"
      EXIT_CODE=2
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
  symbols_version=$(get_api_version_in_symbols_file)
  api_version=$(cat ../core/CORE_API_VERSION.md)
  # Extract major version number from "1.0.0"
  api_version=${api_version%%.*}

  if (( symbols_version == api_version )); then
    output_ok "API version in .symbols file and in CORE_API_VERSION.md is the same"
  else
    output_error "API version in .symbols file and in CORE_API_VERSION.md is different"
    EXIT_CODE=3
  fi
}

verify_action() {
  PKG_NAME=libkeymancore
  LIB_NAME=libkeymancore
  if [[ ! -f debian/${PKG_NAME}.symbols ]]; then
    output_warning "Missing ${PKG_NAME}.symbols file"
    exit 0
  fi

  EXIT_CODE=0
  check_api_not_changed
  check_updated_version_number
  check_for_major_api_changes
  check_for_api_version_consistency
  exit "${EXIT_CODE}"
}

builder_run_action dependencies  dependencies_action
builder_run_action source        source_action
builder_run_action verify        verify_action
