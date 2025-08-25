#!/usr/bin/env bash
# TC build script for Keyman Linux/Release
# shellcheck disable=SC2164

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/build/utils.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-helpers.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-linux.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/linux/linux-actions.inc.sh"

################################ Main script ################################

# shellcheck disable=SC2016
builder_describe \
  "Create a daily release of 'master', 'beta',  or 'stable-*' branch." \
  "all            run all actions" \
  "configure      install dependencies" \
  "build          make a release build" \
  "test           run unit tests" \
  "publish        make a source tarball and publish to downloads and launchpad" \
  "--gpgkey=GPGKEYGRIP                GPG key for signing" \
  "--gpgpw=GPGKEYPW                   GPG key passphrase" \
  "--rsync-path=RSYNC_PATH            rsync path on remote server" \
  "--rsync-user=RSYNC_USER            rsync user on remote server" \
  "--rsync-host=RSYNC_HOST            rsync host on remote server" \
  "--rsync-root=RSYNC_ROOT            rsync root on remote server" \
  "--help.keyman.com=HELP_KEYMAN_COM  path to help.keyman.com repository" \

builder_parse "$@"

cd "${KEYMAN_ROOT}/linux"

function _install_additional_dependencies() {
  builder_echo start additional_dependencies "Installing additional dependencies"

  # Additionally we need quilt to be able to create the source package.
  # Since this is not needed to build the binary package, it is not
  # (and should not be) included in `build-depends` in `debian/control`.
  sudo DEBIAN_FRONTEND=noninteractive apt-get -q -y install quilt

  builder_echo end additional_dependencies success "Finished installing additional dependencies"
}

function _cleanup_before_creating_source_package() {
  # Cleanup before creating the source package.
  builder_echo start cleanup "Cleanup before creating the source package"
  git clean -dxf
  builder_echo end cleanup success "Cleanup before creating the source package"
}

function _make_release_source_tarball() {
  builder_echo start "make source tarball" "Make source tarball"
  rm -rf dist
  ./scripts/reconf.sh
  PKG_CONFIG_PATH="${KEYMAN_ROOT}/core/build/arch/release/meson-private" ./scripts/dist.sh
  mkdir -p "upload/${KEYMAN_VERSION}"
  cp -a dist/*.tar.gz "upload/${KEYMAN_VERSION}"
  (
    cd "upload/${KEYMAN_VERSION}"
    sha256sum ./*.tar.gz > SHA256SUMS
    builder_echo end "make source tarball" success "Make source tarball"
  )
}

function _sign_source_tarball() {
  (
    builder_echo start "sign source tarball" "Sign source tarball"
    cd "upload/${KEYMAN_VERSION}"

    eval "$(gpg-agent -vv --daemon --allow-preset-passphrase --debug-level 9)"
    /usr/lib/gnupg/gpg-preset-passphrase --passphrase "${GPGKEYPW}" --preset "${GPGKEYGRIP}"
    for f in ./*.tar.gz; do gpg --output "${f}.asc" -a --detach-sig "${f}"; done
    /usr/lib/gnupg/gpg-preset-passphrase --forget "${GPGKEYGRIP}" || true
    builder_echo end "sign source tarball" success "Sign source tarball"
  )
}

function _publish_to_downloads() {
  builder_echo start "publish to downloads" "Publish to downloads.keyman.com"

  local UPLOAD_DIR KEYMAN_TGZ

  UPLOAD_DIR="upload/${KEYMAN_VERSION}"
  KEYMAN_TGZ="keyman-${KEYMAN_VERSION}.tar.gz"

  # Set permissions as required on download site
  builder_echo "Setting upload file permissions for downloads.keyman.com"
  chmod g+w  "${UPLOAD_DIR}"
  chmod a+rx "${UPLOAD_DIR}"
  chmod g+w  "${UPLOAD_DIR}"/*
  chmod a+r  "${UPLOAD_DIR}"/*

  write_download_info "${UPLOAD_DIR}" "${KEYMAN_TGZ}" "Keyman for Linux" tar.gz linux
  tc_rsync_upload "${UPLOAD_DIR}" "linux/${KEYMAN_TIER}"

  builder_echo end "publish to downloads" success "Publish to downloads.keyman.com"
}

function _publish_to_launchpad() {
  builder_echo start "upload to launchpad" "Upload to launchpad"

  git reset --hard

  /usr/lib/gnupg/gpg-preset-passphrase --passphrase "${GPGKEYPW}" --preset "${GPGKEYGRIP}"
  UPLOAD=yes "${KEYMAN_ROOT}/linux/scripts/launchpad.sh"
  /usr/lib/gnupg/gpg-preset-passphrase --forget "${GPGKEYGRIP}" || true

  builder_echo end "upload to launchpad" success "Upload to launchpad"
}

function _publish_linux_help() {
  builder_echo start "upload linux help" "Upload new Keyman Linux help to help.keyman.com"

  # shellcheck disable=SC2016
  git config credential.helper '!f() { sleep 1; echo "username=${GITHUB_USER}"; echo "password=${GITHUB_TOKEN}"; }; f'
  "${KEYMAN_ROOT}/resources/build/ci/help-keyman-com.sh" linux

  builder_echo end "upload linux help" success "Upload new Keyman Linux help to help.keyman.com"
}

function publish_action() {
  _cleanup_before_creating_source_package
  _make_release_source_tarball
  _sign_source_tarball
  _publish_to_downloads
  _publish_to_launchpad
  _publish_linux_help
}

if builder_has_action all; then
  linux_install_dependencies_action
  _install_additional_dependencies

  tc_set_variables_for_nvm

  linux_build_action
  linux_unit_tests_action --no-integration
  publish_action
else
  builder_run_action  configure   linux_install_dependencies_action
  builder_run_action  configure   _install_additional_dependencies

  tc_set_variables_for_nvm

  builder_run_action  build       linux_build_action
  builder_run_action  test        linux_unit_tests_action --no-integration
  builder_run_action  publish     publish_action
fi
