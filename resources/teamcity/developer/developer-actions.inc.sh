#!/usr/bin/env bash

developer_install_dependencies_on_linux_action() {
  builder_echo start "install dependencies" "Installing dependencies"

  if is_ubuntu; then
    linux_check_and_install_packages devscripts jq meson
  elif is_macos; then
    macos_install_packages bash jq python3 meson ninja coreutils pyenv
  fi

  install_nvm
  install_emscripten

  builder_echo end "install dependencies" success "Finished installing dependencies"
}
