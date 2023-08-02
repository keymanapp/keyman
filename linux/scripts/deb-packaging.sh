#!/usr/bin/env bash
# Actions for creating a Debian source package. Used by deb-packaging.yml GHA.

set -eu

# TEST FAILING packaging
exit 3

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe \
  "Helper for building Debian packages." \
  "dependencies              Install dependencies as found in debian/control" \
  "source+                   Build source package" \
  "verify                    Verify API" \
  "--gha                     Build from GitHub Action"

builder_parse "$@"

cd "$REPO_ROOT/linux"

if builder_has_option --gha; then
  START_STEP="::group::${COLOR_GREEN}"
  END_STEP="::endgroup::"
else
  START_STEP="${COLOR_GREEN}"
  END_STEP=""
fi

if builder_start_action dependencies; then
  sudo mk-build-deps --install --tool='apt-get -o Debug::pkgProblemResolver=yes --no-install-recommends --yes' debian/control
  builder_finish_action success dependencies
  exit 0
fi

if builder_start_action source; then
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

  builder_finish_action success source
  exit 0
fi

if builder_start_action verify; then
  tar xf "${SRC_PKG}"
  if [ ! -f debian/libkmnkbp0-0.symbols ]; then
    echo ":warning: Missing libkmnkbp0-0.symbols file" >&2
  else
    tmpDir=$(mktemp -d)
    dpkg -x "${BIN_PKG}" "$tmpDir"
    cd debian
    dpkg-gensymbols -v"${PKG_VERSION}" -plibkmnkbp0-0 -e"${tmpDir}"/usr/lib/x86_64-linux-gnu/libkmnkbp0.so* -Olibkmnkbp0-0.symbols -c4
    echo ":heavy_check_mark: libkmnkbp0-0 API didn't change" >&2
  fi
  builder_finish_action success verify
  exit 0
fi
