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
  local includes=("${!includes_array}")
  local excludes_array="${excludes_var}[@]"
  local excludes=("${!excludes_array}")
  local dir all_dirs found_match inc

  mapfile -t all_dirs < <(find "${directory}" -mindepth 1 -maxdepth 1 -type d | sort)
  for dir in "${all_dirs[@]}"; do
    found_match=false
    for inc in "${includes[@]}"; do
      if [[ "./${inc}" =~ ^${dir} ]]; then
        found_match=true
        if [[ "./${inc}" != "${dir}" ]]; then
          # check subdirectories
          generate_tar_ignore_list "${dir}" "${includes_var}" "${excludes_var}" "${list_var}" "${prefix}"
        fi
        # check if files/subdir in $dir are in excludes list
        _generate_excludes_for_dir "${dir}" "${includes_var}" "${excludes_var}" "${list_var}"
        break
      fi
    done
    if ! ${found_match}; then
      _add_to_list "${list_var}" "${dir}"
    fi
  done
}

function _generate_excludes_for_dir() {
  local directory="$1"
  local includes_var="$2"
  local excludes_var="$3"
  local list_var="$4"
  local includes_array="${includes_var}[@]"
  local includes=("${!includes_array}")
  local excludes_array="${excludes_var}[@]"
  local excludes=("${!excludes_array}")
  local file all_files excluded included is_match

  mapfile -t all_files < <(find "${directory}" -mindepth 1 -maxdepth 1 | sort)
  is_match=false
  for file in "${all_files[@]}"; do
    for included in "${includes[@]}"; do
      if [[ "${file}" == ./${included} ]]; then
        is_match=true
        break
      fi
    done
    if ${is_match} ; then
      if [[ "${file}" != ./${included} ]] && [[ -f "${file}" ]]; then
      _add_to_list "${list_var}" "${file}"
      fi
    else
      for excluded in "${excludes[@]}"; do
        if [[ "${file}" == ./${excluded} ]]; then
          _add_to_list "${list_var}" "${file}"
          break
        elif [[ "./${excluded}" =~ ^${file} ]]; then
          # check subdirectories
          _generate_excludes_for_dir "${file}" "${includes_var}" "${excludes_var}" "${list_var}"
          break
        fi
      done
    fi
  done
}

function _add_to_list() {
  local list_var="$1"
  local filename="$2"

  # Note: the files end up in subdirectories under `keyman` (or rather
  # the directory name of $KEYMAN_ROOT), so we can
  # include that when matching files and directories to ignore.
  # shellcheck disable=SC2154
  eval "${list_var}+=(\"--tar-ignore=${prefix}/${filename#./}\")"
}
