#!/usr/bin/env bash
# This script serves two purposes:
# - Verify Keyman builds from source tarball
# - Verify source package (that will be uploaded to Launchpad) does not
#   produce lintian errors

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder-basic.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

VERIFY_SOURCE=true
VERIFY_LAUNCHPAD=true
CREATE_TARBALL=true

while (( $# > 0)); do
  case "$1" in
    --no-create-tarball)
      CREATE_TARBALL=false
      shift
      ;;
    --source-only)
      VERIFY_LAUNCHPAD=false
      shift
      ;;
    --launchpad-only)
      VERIFY_SOURCE=false
      shift
      ;;
    --help|-h)
      echo "Usage: $0 [--source-only|--launchpad-only] [--no-create-tarball] [<target directory>]"
      exit 0
      ;;
    *)
      break
      ;;
  esac
done

TARGET_DIR="${1:-/tmp/keyman-${KEYMAN_VERSION}}"

create_source_tarball() {
  local target_dir="$1"

  builder_echo "Create source tarball"
  rm -rf dist
  ./scripts/reconf.sh
  PKG_CONFIG_PATH="${KEYMAN_ROOT}/core/build/arch/release/meson-private" ./scripts/dist.sh

  mkdir -p "${target_dir}"
  cp dist/keyman-*.tar.xz "${target_dir}"
}

extract_source_tarball() {
  local target_dir="$1"

  builder_echo "Extract source tarball"
  cd "${target_dir}"
  tar -xvf keyman-*.tar.xz
}

verify_can_build() {
  local target_dir="$1"
  builder_echo heading "Verifying build of tarball"
  cd "${target_dir}"
  # running `npm i` should happen automatically. See #15905
  npm i
  ./build.sh configure,build
}

create_source_package() {
  local target_dir="$1"

  # Production code uses uscan to download the latest tarball from
  # downloads.keyman.com/linux and then basically copies the debian
  # directory from the source repo. Since we want to work on code that
  # is not released yet we do similar steps here and then skip the
  # download in the launchpad script.
  cd "${target_dir}"
  mkdir -p launchpad
  cd launchpad
  cp -r "../keyman" "keyman-${KEYMAN_VERSION}"
  cp -r "${KEYMAN_ROOT}/linux/debian" "keyman-${KEYMAN_VERSION}"
  cp "${target_dir}/keyman-${KEYMAN_VERSION}.tar.xz" "keyman_${KEYMAN_VERSION}.orig.tar.xz"
  "keyman-${KEYMAN_VERSION}/linux/scripts/launchpad.sh" --no-download --no-upload \
    --dist "$(lsb_release -c -s)" --outputdir "${target_dir}/launchpad" --no-lintian
}

verify_lintian() {
  builder_echo heading "Running lintian"
  lintian "keyman_${KEYMAN_VERSION}"*source.changes
}

cd "${KEYMAN_ROOT}/linux"

if ${CREATE_TARBALL}; then
  builder_echo start tarball "Building source tarball"
  create_source_tarball "${TARGET_DIR}"
  builder_echo end tarball success "Finished building source tarball"
fi

if ${VERIFY_SOURCE}; then
  builder_echo start verifySource "Verifying source tarball"
  extract_source_tarball "${TARGET_DIR}"
  verify_can_build "${TARGET_DIR}/keyman"
  rm -rf "${TARGET_DIR}/keyman"
  builder_echo end verifySource success "Finished verifying source tarball"
fi

if ${VERIFY_LAUNCHPAD}; then
  builder_echo start lintian "Verifying Launchpad source package"
  extract_source_tarball "${TARGET_DIR}"
  create_source_package "${TARGET_DIR}"
  verify_lintian
  rm -rf "${TARGET_DIR}/keyman"
  rm -rf "${TARGET_DIR}/launchpad"
  builder_echo end lintian success "Finished verifying Launchpad source package"
fi
