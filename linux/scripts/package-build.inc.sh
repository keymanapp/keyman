# shellcheck shell=bash
# Keyman is copyright (C) SIL Global. MIT License.

function checkPrerequisites() {
  if [[ "${UPLOAD:=}" == "yes" ]]; then
    SIM=""
  else
    SIM="-s"
  fi

  # Check the tier
  if [[ -z "${KEYMAN_TIER:=}" ]]; then
    echo "TIER.md or \${KEYMAN_TIER} must be set to (alpha, beta, stable) to use this script"
    exit 1
  fi

  if ! command -v xmllint > /dev/null; then
    echo "you must install xmllint (libxml2-utils package) to use this script"
    exit 1
  fi

  # shellcheck disable=SC2034
  projects="${PROJECT:=keyman}"
}

function downloadSource() {
  local packageDir
  packageDir=$1

  if [[ "${proj:=}" == "keyman" ]]; then
    # shellcheck disable=SC2154
    cd "${BASEDIR}" || exit
    ./build.sh clean
  fi

  # Update tier in Debian watch files (replacing any previously set tier) and remove comment
  sed -e "s/\$tier\|alpha\|beta\|stable/${KEYMAN_TIER}/g" -e "s/^# .*$//" "${BASEDIR}"/scripts/watch.in > debian/watch

  version=$(uscan --report --dehs|xmllint --xpath "//dehs/upstream-version/text()" -)
  dirversion=$(uscan --report --dehs|xmllint --xpath "//dehs/upstream-url/text()" - | cut -d/ -f6)
  echo "${proj} version is ${version}"
  uscan || (echo "ERROR: No new version available for ${proj}" >&2 && exit 1)
  cd ..
  mv "${proj}-${version}" "${BASEDIR}/${packageDir}"
  mv "${proj}_${version}.orig.tar.gz" "${BASEDIR}/${packageDir}"
  mv "${proj}-${version}.tar.gz" "${BASEDIR}/${packageDir}"
  mv "${proj}"*.asc "${BASEDIR}/${packageDir}"
  rm "${proj}"*.debian.tar.xz
  cd "${BASEDIR}/${packageDir}" || exit
  wget -N "https://downloads.keyman.com/linux/${KEYMAN_TIER}/${dirversion}/SHA256SUMS"
  sha256sum -c --ignore-missing SHA256SUMS | grep "${proj}"
}

function wait_for_apt_deb {
  # from https://gist.github.com/hrpatel/117419dcc3a75e46f79a9f1dce99ef52
  while sudo fuser /var/{lib/{dpkg,apt/lists},cache/apt/archives}/lock &>/dev/null 2>&1; do
    echo "Waiting for apt/dpkg lock to release, sleeping 10s"
    sleep 10
  done
}

function checkAndInstallRequirements()
{
  local TOINSTALL=""

  for p in devscripts equivs python3-dev
  do
    if ! dpkg -s "${p}" >/dev/null 2>&1; then
      TOINSTALL="${TOINSTALL} ${p}"
    fi
  done

  if [[ -n "${TOINSTALL}" ]]; then
    wait_for_apt_deb && sudo apt-get update
    # shellcheck disable=SC2086
    wait_for_apt_deb && sudo DEBIAN_FRONTEND="noninteractive" \
      apt-get -qy install ${TOINSTALL}
  fi

  sudo mk-build-deps debian/control
  wait_for_apt_deb && sudo DEBIAN_FRONTEND="noninteractive" \
    apt-get -qy --allow-downgrades install ./keyman-build-deps_*.deb
  rm -f keyman-build-deps_*
}

# Generate an array of files and directories to ignore, prefixed with --tar-ignore=keyman/
function generate_tar_ignore_list() {
  local directory="$1"
  local includes_var="$2"
  local excludes_var="$3"
  local list_var="$4"
  local prefix="$5"
  local includes_array="${includes_var}[@]"
  # shellcheck disable=SC2034
  local includes=("${!includes_array}")
  local excludes_array="${excludes_var}[@]"
  local excludes=("${!excludes_array}")

  # Loop through excludes and put all without path in single_excludes
  local single_excludes=()
  for item in "${excludes[@]}"; do
    if [[ ${item} != */* ]]; then
        single_excludes+=("${item}")
    fi
  done

  local ignore_list=()
  _process_directory "${directory}" false

  for item in "${ignore_list[@]}"; do
    eval "${list_var}+=(\"--tar-ignore=${item}\")"
  done
}

function _process_directory() {
  local directory="$1"
  local isParentIncluded="$2"
  local all_items item

  mapfile -t all_items < <(find "${directory}" -mindepth 1 -maxdepth 1 | sort)
  for item in "${all_items[@]}"; do
    debug "Checking item: ${item}"
    if _is_exact_match "includes" "${item}"; then
      debug "  Including (full match): ${item}"
      if [[ -d "${item}" ]]; then
        _process_directory "${item}" true
      fi
    elif _starts_with "includes" "${item}"; then
      debug "  Including (partial match): ${item}"
      if [[ -d "${item}" ]]; then
        _process_directory "${item}" "${isParentIncluded}"
      fi
    elif _is_exact_match "excludes" "${item}"; then
      debug "  Excluding (full match): ${item}"
      _add_to_list "${item}"
    elif _ends_with "single_excludes" "${item}" || _is_wildcard_match "single_excludes" "${item}"; then
      debug "  Excluding (single exclude): ${item}"
      _add_to_list "${item}"
    elif [[ "${isParentIncluded}" == "false" ]]; then
      debug "  Excluding (not included): ${item}"
      _add_to_list "${item}"
    else
      debug "  Including (parent included): ${item}"
      if [[ -d "${item}" ]]; then
        _process_directory "${item}" "${isParentIncluded}"
      fi
    fi
  done
}

function _add_to_list() {
  local item="$1"
  ignore_list+=("${prefix}/${item#./}")
}

# Returns true if one of the values in the $1 array equals ${file} ($2)
# Example: will return true for array=(path1/path2) file=./path1/path2
function _is_exact_match() {
  local array_var="$1"
  local file="$2"
  local array_name="${array_var}[@]"
  local haystack=("${!array_name}")
  local item
  for item in "${haystack[@]}"; do
    if [[ "./${item#./}" == "${file}" ]]; then
      return 0
    fi
  done
  return 1
}

# Returns true if one of the values in the $1 array starts with ${file} ($2)
# Example: will return true for array=(path1/path2) file=./path1
function _starts_with() {
  local array_var="$1"
  local file="$2"
  local array_name="${array_var}[@]"
  local array_values=("${!array_name}")
  local array_item
  for array_item in "${array_values[@]}"; do
    if [[ "./${array_item#./}" == ${file}* ]]; then
      return 0
    fi
  done
  return 1
}

# Returns true if ${file} ($2) ends with one of the values in the $1 array
# Example: will return true for array=(path2) file=./path1/path2
function _ends_with() {
  local array_var="$1"
  local file="$2"
  local array_name="${array_var}[@]"
  local array_values=("${!array_name}")
  local array_item
  for array_item in "${array_values[@]}"; do
    if [[ "${file}" == */${array_item#./} ]]; then
      return 0
    fi
  done
  return 1
}

# Returns true if ${file} ($2) matches one of the values of the $1 array.
# These values may contain wildcards.
# Example: will return true for array=(*.sh) file=./path1/build.sh
function _is_wildcard_match() {
  local array_var="$1"
  local file="$2"
  local array_name="${array_var}[@]"
  local array_values=("${!array_name}")
  local array_item
  for array_item in "${array_values[@]}"; do
    array_item=${array_item/./\\.}
    if [[ "${file}" =~ /${array_item/\*/.\*}$ ]]; then
      return 0
    fi
  done
  return 1
}

function debug() {
  # echo "$@"
  return 0
}
