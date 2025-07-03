#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.

developer_install_dependencies_on_linux_action() {
  if ! is_ubuntu; then
    return 0
  fi

  builder_echo start "install dependencies" "Installing dependencies"

  ba_linux_check_and_install_packages devscripts jq meson
  ba_linux_install_nvm
  ba_linux_install_emscripten

  builder_echo end "install dependencies" success "Finished installing dependencies"
}
