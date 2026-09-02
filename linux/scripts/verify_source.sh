#!/usr/bin/env bash
# This script serves two purposes:
# - Verify Keyman builds from source tarball
# - Verify source package (that will be uploaded to Launchpad) does not
#   produce lintian errors

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
# shellcheck source=resources/build/builder-full.inc.sh
. "${THIS_SCRIPT%/*}/../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "${KEYMAN_ROOT}/resources/locate_emscripten.inc.sh"

builder_describe \
  "Verify source tarball and source package" \
  build \
  "--no-create-tarball   Skip creating a new tarball. Instead use existing one." \
  "--source-only         Only check that it's possible to build Keyman from the source tarball." \
  "--launchpad-only      Only check that lintian doesn't produce errors when run on the source package." \
  "--target-dir=TARGET_DIR  Directory where to put generate files. Default: /tmp/keyman-<version>."

builder_parse "$@"

if ! builder_has_option --target-dir; then
  TARGET_DIR="/tmp/keyman-${KEYMAN_VERSION}"
fi

create_source_tarball() {
  local target_dir="$1"

  builder_echo heading "Create source tarball"
  mkdir -p "${target_dir}"
  rm -rf dist
  ./scripts/reconf.sh
  PKG_CONFIG_PATH="${KEYMAN_ROOT}/core/build/arch/release/meson-private" ./scripts/dist.sh
  mv "dist/keyman-${KEYMAN_VERSION}.tar.xz" "${target_dir}"
}

extract_source_tarball() {
  local target_dir="$1"

  builder_echo heading "Extract source tarball"
  cd "${target_dir}"
  rm -rf "keyman"
  tar -xvf "keyman-${KEYMAN_VERSION}.tar.xz"
}

verify_can_build() {
  local target_dir="$1"
  builder_echo heading "Verifying build of tarball"
  cd "${target_dir}"
  ./build.sh configure,build
}

create_source_package() {
  local target_dir="$1"

  builder_echo heading "Creating source package"
  # Production code uses uscan to download the latest tarball from
  # downloads.keyman.com/linux and then basically copies the debian
  # directory from the source repo. Since we want to work on code that
  # is not released yet we do similar steps here and then skip the
  # download in the launchpad script.
  cd "${target_dir}"
  mkdir -p launchpad
  cd launchpad
  cp -r "../keyman-${KEYMAN_VERSION}" .
  cp -r "${KEYMAN_ROOT}/linux/debian" "keyman-${KEYMAN_VERSION}"
  cp "${target_dir}/keyman-${KEYMAN_VERSION}.tar.xz" "keyman_${KEYMAN_VERSION}.orig.tar.xz"
  "keyman-${KEYMAN_VERSION}/linux/scripts/launchpad.sh" --no-download \
    --dist "$(lsb_release -c -s)" --outputdir "${target_dir}/launchpad" --no-lintian --no-sign
}

verify_lintian() {
  builder_echo heading "Running lintian"
  lintian "keyman_${KEYMAN_VERSION}"*source.changes
}

install_emscripten() {
  local target_dir="$1"
  if [[ ! -d "${target_dir}/emsdk" ]]; then
    builder_echo heading "Installing Emscripten for build verification"
    install_emscripten_into "${target_dir}/emsdk"
  else
    builder_echo heading "Emscripten already exists in ${target_dir}/emsdk, skipping installation"
  fi
  EMSCRIPTEN_BASE="${target_dir}/emsdk/upstream/emscripten"
  export EMSCRIPTEN_BASE
}

cd "${KEYMAN_ROOT}/linux"

if ! builder_has_option --no-create-tarball; then
  builder_echo start tarball "Building source tarball"
  create_source_tarball "${TARGET_DIR}"
  builder_echo end tarball success "Finished building source tarball"
fi

if ! builder_has_option --launchpad-only; then
  builder_echo start verifySource "Verifying source tarball"
  extract_source_tarball "${TARGET_DIR}"
  if builder_is_ci_test_build; then
    install_emscripten "${TARGET_DIR}"
  fi
  verify_can_build "${TARGET_DIR}/keyman"
  rm -rf "${TARGET_DIR}/keyman"
  builder_echo end verifySource success "Finished verifying source tarball"
fi

if ! builder_has_option --source-only; then
  builder_echo start lintian "Verifying Launchpad source package"
  extract_source_tarball "${TARGET_DIR}"
  mv "${TARGET_DIR}/keyman" "${TARGET_DIR}/keyman-${KEYMAN_VERSION}"
  create_source_package "${TARGET_DIR}"
  verify_lintian
  rm -rf "${TARGET_DIR}/keyman-${KEYMAN_VERSION}"
  rm -rf "${TARGET_DIR}/launchpad"
  builder_echo end lintian success "Finished verifying Launchpad source package"
fi
