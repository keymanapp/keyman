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
  "--project=PROJECT                Only upload this project. Default: keyman" \
  "--dist=DIST                      Only upload this distribution. Default: upload all supported dists." \
  "--packageversion=PACKAGEVERSION  String to append to the package version. Default: '1~sil1'." \
  "--outputdir=OUTPUTDIR            Directory for resulting artifacts. Default: \$KEYMAN_ROOT/linux/launchpad."

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

distributions="${DIST:-jammy noble questing resolute}"
packageversion="${PACKAGEVERSION:-1~sil1}"

if ! builder_has_option --no-download; then
  rm -rf launchpad
  mkdir -p launchpad
fi

for proj in ${projects:-}; do
  if ! builder_has_option --no-download; then
    downloadSource launchpad
  else
    version=$(cat "${KEYMAN_ROOT}/VERSION.md")
    cd "${OUTPUTDIR}"
  fi

  cd "${proj}-${version:-}"
  pwd
  cp debian/changelog "../${proj}-changelog"
  for dist in ${distributions}; do
    cp "../${proj}-changelog" debian/changelog
    dch -v "${version}-${packageversion}~${dist}" "source package for PPA"
    dch -D "${dist}" -r ""
    # shellcheck disable=SC2248  # no quotes for $LINTIAN_OPTS - might be empty string
    debuild ${LINTIAN_OPTS} -d -S -sa -Zxz
  done
  if builder_has_option --upload || builder_has_option --simulate; then
    cd ..
    for dist in ${distributions}; do
      # shellcheck disable=SC2248  # no quotes for $SIM - it might not be set
      dput ${SIM:-} "${ppa}" "${proj}_${version}-${packageversion}~${dist}_source.changes"
    done
  fi
  cd "${KEYMAN_ROOT}/linux"
done
