# shellcheck shell=bash
# Keyman is copyright (C) SIL Global. MIT License.

ios_build() {
  builder_echo start "build" "Building KeymanEngine + Keyman for iOS"
  # shellcheck disable=SC2154
  "${KEYMAN_ROOT}/ios/ci.sh" build
  builder_echo end "build" success "Finished building KeymanEngine + Keyman for iOS"
}

ios_capture_build_artifacts() {
  builder_echo start "prep-release" "Using prep-release script to capture build artifacts"
  "${KEYMAN_ROOT}/ios/tools/prepRelease.sh"
  builder_echo end "prep-release" success "Finished capturing build artifacts"
}
