# shellcheck shell=bash
# Keyman is copyright (C) SIL Global. MIT License.

macos_clean_action() {
  ba_mac_clean_xcode_derived_data
  ba_mac_unmount_volumes_keyman
}

macos_build_action() {
  builder_echo start "build" "Building Keyman for macOS"
  # shellcheck disable=SC2154
  builder_launch /mac/build.sh configure build test publish
  builder_echo end "build" success "Finished building Keyman for macOS"
}
