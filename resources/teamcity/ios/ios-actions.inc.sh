#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.

ios_unlock_keychain() {
  if [[ -z "${MAC_BUILDAGENT_PASSWORD}" ]]; then
    echo "Error: MAC_BUILDAGENT_PASSWORD environment variable is not set."
    exit 1
  fi
  security unlock-keychain -p "${MAC_BUILDAGENT_PASSWORD}" login.keychain
}

ios_clean_action() {
  builder_echo start "clean" "Cleaning XCode DerivedData mess"
  rm -rf "${HOME}/Library/Developer/Xcode/DerivedData"
  builder_echo end "clean" success "Finished cleaning XCode DerivedData mess"
}

ios_build() {
  builder_echo start "build" "Building KeymanEngine + Keyman for iOS"
  # shellcheck disable=SC2154
  "${KEYMAN_ROOT}/ios/ci.sh" build
  builder_echo end "build" success "Finished building KeymanEngine + Keyman for iOS"
}

ios_capture_build_artifacts() {
  builder_echo start "prep-release" "Using prep-release script to capture build artifacts"
  "${KEYMAN_ROOT}/ios/tools/prep-release.sh"
  builder_echo end "prep-release" success "Finished capturing build artifacts"
}
