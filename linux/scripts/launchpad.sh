#!/usr/bin/env bash
# Build source packages from nightly builds and upload to PPA

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
# shellcheck source=resources/build/builder-full.inc.sh
. "${THIS_SCRIPT%/*}/../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck source=linux/scripts/package-build.inc.sh
. "$(dirname "${THIS_SCRIPT}")/package-build.inc.sh"

builder_describe \
  "Build source packages from nightly builds and upload to PPA" \
  build \
  "--no-download                    Don't download source. Assume keyman-<version> exists as subdirectory of current dir." \
  "--upload                         Upload to launchpad." \
  "--simulate                       Simulate the upload to launchpad." \
  "--no-lintian                     Don't run lintian while creating source package." \
  "--dist=DIST                      Only upload this distribution. Default: upload all supported dists." \
  "--packageversion=PACKAGEVERSION  String to append to the package version. Default: '1~sil1'." \
  "--outputdir=OUTPUTDIR            Directory for resulting artifacts. Default: \$KEYMAN_ROOT/linux/launchpad." \
  "--retries=RETRIES                Number of retries on network errors. Default: 5."

builder_parse "$@"

cd "${KEYMAN_ROOT}/linux"

checkPrerequisites

if [[ -z "${OUTPUTDIR:-}" ]]; then
  OUTPUTDIR="${KEYMAN_ROOT}/linux/launchpad"
fi

if builder_has_option --simulate; then
  SIM="-s"
else
  SIM=""
fi

if builder_has_option --no-lintian; then
  LINTIAN_OPTS="--no-lintian"
else
  LINTIAN_OPTS=""
fi

if [[ "${KEYMAN_TIER}" == "stable" ]]; then
  ppa="ppa:keymanapp/keyman"
elif [[ "${KEYMAN_TIER}" == "beta" ]]; then
  ppa="ppa:keymanapp/keyman-beta"
else
  ppa="ppa:keymanapp/keyman-alpha"
fi
echo "ppa: ${ppa}"

function upload_with_retry() {
  local changes_file="$1"
  local retries="$2"
  local sim="$3"
  local ppa="$4"
  local attempt=0
  local max_attempts=$((retries+1))
  local rc=0
  local output

  while (( attempt++ < max_attempts )); do
    builder_echo "Uploading ${changes_file} (attempt ${attempt}/${max_attempts})..."
    set -o pipefail
    # shellcheck disable=SC2312,SC2086  # no quotes for $sim - it might not be set
    output=$(dput ${sim:-} "${ppa}" "${changes_file}" 2>&1 | tee /dev/stderr)
    rc=$?
    set +o pipefail
    if [[ ${rc} == 0 ]]; then
      return 0
    fi
    # Check if error is a network-related error
    if [[ ${rc} == 1 ]] && echo "${output}" | grep -iE 'Connection reset|Connection refused|Network is unreachable|Name or service not known|Connection timed out|Temporary failure|Broken pipe|Connection aborted' > /dev/null 2>&1; then
      if (( attempt < max_attempts )); then
        builder_echo warning "Network error detected, retrying..."
        sleep $((attempt * 5))
      fi
    else
      # Non-network error, fail immediately
      builder_echo error "Upload failed with non-network error (exit code: ${rc})"
      builder_echo error "${output}"
      return "${rc}"
    fi
  done

  builder_echo error "Failed to upload after ${max_attempts} attempts"
  return 1
}

distributions="${DIST:-jammy noble questing resolute}"
packageversion="${PACKAGEVERSION:-1~sil1}"
retries="${RETRIES:-5}"

if ! builder_has_option --no-download; then
  rm -rf launchpad
  mkdir -p launchpad
fi

if ! builder_has_option --no-download; then
  downloadSource launchpad
else
  version=$(cat "${KEYMAN_ROOT}/VERSION.md")
  cd "${OUTPUTDIR}"
fi

cd "keyman-${version:-}"
pwd
cp debian/changelog "../keyman-changelog"
for dist in ${distributions}; do
  cp "../keyman-changelog" debian/changelog
  dch -v "${version}-${packageversion}~${dist}" "source package for PPA"
  dch -D "${dist}" -r ""
  # shellcheck disable=SC2248  # no quotes for $LINTIAN_OPTS - might be empty string
  debuild ${LINTIAN_OPTS} -d -S -sa -Zxz
done
if builder_has_option --upload || builder_has_option --simulate; then
  cd ..
  for dist in ${distributions}; do
    if ! upload_with_retry "keyman_${version}-${packageversion}~${dist}_source.changes" "${retries}" "${SIM}" "${ppa}"; then
      builder_die "Upload failed for distribution ${dist}"
    fi
  done
fi
