#!/usr/bin/env bash
# TC build script for Keyman Linux/Release
# shellcheck disable=SC2164

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/shellHelperFunctions.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-actions.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-linux.inc.sh"

################################ Main script ################################

# shellcheck disable=SC2016
builder_describe \
  "Create a daily release of 'master', 'beta',  or 'stable-*' branch." \
  "all            run all actions" \
  "configure      install dependencies" \
  "build          make a release build" \
  "test           run unit tests" \
  "publish        make a source tarball" \
  "--gpgkey=GPGKEYGRIP      GPG key for signing" \
  "--gpgpw=GPGKEYPW         GPG key passphrase" \
  "--rsync-path=RSYNC_PATH  rsync path on remote server" \
  "--rsync-user=RSYNC_USER  rsync user on remote server" \
  "--rsync-host=RSYNC_HOST  rsync host on remote server" \
  "--rsync-root=RSYNC_ROOT  rsync root on remote server"


builder_parse "$@"

cd "${KEYMAN_ROOT}/linux"

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

  local DATE, UPLOAD_BASE, UPLOAD_FOLDER, UPLOAD_DIR
  local ARTIFACTS, NAMES
  local TAR_GZ, HASH, SIZE, DOWNLOAD_INFO

  DATE=$(date +%F)

  UPLOAD_BASE="upload"
  UPLOAD_FOLDER="${KEYMAN_VERSION}"
  UPLOAD_DIR="${UPLOAD_BASE}/${UPLOAD_FOLDER}"

  # Set permissions as required on download site
  builder_echo "Setting upload file permissions for downloads.keyman.com"
  chmod a+rx "${UPLOAD_DIR}"
  chmod g+w "${UPLOAD_DIR}"

  chmod g+rw "${UPLOAD_DIR}"/*
  chmod a+r  "${UPLOAD_DIR}"/*

  ARTIFACTS=("keyman-${KEYMAN_VERSION}.tar.gz")
  NAMES=("Keyman for Linux")

  for i in "${!ARTIFACTS[@]}"; do
      TAR_GZ=${ARTIFACTS[${i}]}
      # Construct .download_info
      HASH=$(md5sum "${UPLOAD_DIR}/${TAR_GZ}" | cut -d ' ' -f 1)
      SIZE=$(stat --print="%s" "${UPLOAD_DIR}/${TAR_GZ}")
      # TODO: Truncate NAME
      DOWNLOAD_INFO=$( jq -n \
      --arg NAME "${NAMES[${i}]}" \
      --arg BUILD_NUMBER "${KEYMAN_VERSION}" \
      --arg DATE "${DATE}" \
      --arg KEYMAN_TIER "${KEYMAN_TIER}" \
      --arg FILENAME "${TAR_GZ}" \
      --arg HASH "${HASH}" \
      --arg BUILD_COUNTER "${KEYMAN_VERSION_PATCH}" \
      --arg SIZE "${SIZE}" \
      '{name: $NAME, version: $BUILD_NUMBER, date: $DATE, platform: "linux", stability: $KEYMAN_TIER, file: $FILENAME, md5: $HASH, type: "tar.gz", build: $BUILD_COUNTER, size: $SIZE}' )
      echo "${DOWNLOAD_INFO}" | jq . >> "${UPLOAD_DIR}/${TAR_GZ}.download_info"
  done

  # Expanded, documented form of the arguments
  # ==========================================
  # "-vrzltp "   # verbose, recurse, zip, copy symlinks, preserve times, permissions
  # "--perms "   # perfectly matches existing file permissions on the build agent
  # "--stats "   # show statistics for log
  # "--rsync-path=\"sudo -u vu2009 rsync\" # path on remote server
  # "--rsh=ssh " # use ssh

  # The actual rsync call.
  # We run into weird quote-based issues if we don't do a monolithic call as seen below, at least at present.
  builder_echo "Performing rsync call"
  rsync -vrzltp --perms --stats --rsync-path="${RSYNC_PATH}" --rsh=ssh "${UPLOAD_DIR}" "${RSYNC_USER}@${RSYNC_HOST}:${RSYNC_ROOT}/linux/${KEYMAN_TIER}"

  builder_echo end "publish to downloads" success "Publish to downloads.keyman.com"
}

function _publish_to_launchpad() {
  builder_echo start "upload to launchpad" "Upload to launchpad"

  git reset --hard

  /usr/lib/gnupg/gpg-preset-passphrase --passphrase "${GPGKEYPW}" --preset "${GPGKEYGRIP}"
  UPLOAD=yes scripts/launchpad.sh
  /usr/lib/gnupg/gpg-preset-passphrase --forget "${GPGKEYGRIP}" || true

  builder_echo end "upload to launchpad" success "Upload to launchpad"
}

function _publish_linux_help() {
  builder_echo start "upload linux help" "Upload new Keyman Linux help to help.keyman.com"

  (
    cd "${KEYMAN_ROOT}/../help.keyman.com" || exit 1
    # shellcheck disable=SC2016
    git config credential.helper '!f() { sleep 1; echo "username=${GITHUB_USER}"; echo "password=${GITHUB_TOKEN}"; }; f'
    cd "${KEYMAN_ROOT}/resources/build"
    ./help-keyman-com.sh linux
  )

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

  set_variables_for_nvm

  linux_build_action
  linux_unit_tests_action --no-integration
  publish_action
else
  builder_run_action  configure   linux_install_dependencies_action

  set_variables_for_nvm

  builder_run_action  build       linux_build_action
  builder_run_action  test        linux_unit_tests_action --no-integration
  builder_run_action  publish     publish_action
fi
