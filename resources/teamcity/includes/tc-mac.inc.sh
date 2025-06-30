#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.

ba_mac_unlock_keychain() {
  if [[ -z "${MAC_BUILDAGENT_PASSWORD}" ]]; then
    echo "Error: MAC_BUILDAGENT_PASSWORD environment variable is not set."
    exit 1
  fi
  security unlock-keychain -p "${MAC_BUILDAGENT_PASSWORD}" login.keychain
}

ba_mac_clean_xcode_derived_data() {
  builder_echo start "clean" "Cleaning XCode DerivedData mess"
  rm -rf "${HOME}/Library/Developer/Xcode/DerivedData"
  builder_echo end "clean" success "Finished cleaning XCode DerivedData mess"
}

ba_mac_unmount_volumes_keyman() {
  builder_echo start "unmount" "Unmounting Keyman volumes"
  if [[ -d /Volumes/Keyman ]]; then
    hdiutil detach /Volumes/Keyman;
  fi
  builder_echo end success "unmount" "Finished unmounting Keyman volumes"
}
