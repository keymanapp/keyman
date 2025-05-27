#!/usr/bin/env bash

macos_install_packages() {
  if ! is_macos; then
    return 0
  fi

  builder_echo start "install packages" "Installing packages on macOS: $*"

  if ! command -v brew &> /dev/null; then
    NONINTERACTIVE=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    # Unfortunately Homebrew gets installed in different directories
    # depending on the architecture. This uses the paths that `install.sh`
    # has.
    if [[ "$(uname -m)" == "arm64" ]]; then
      # On ARM macOS Homebrew gets installed to /opt/homebrew
      HOMEBREW_PREFIX="/opt/homebrew"
    else
      # On Intel macOS Homebrew gets installed to /usr/local
      HOMEBREW_PREFIX="/usr/local"
    fi
    eval "$(${HOMEBREW_PREFIX}/bin/brew shellenv)"
  fi
  brew install "$@"

  builder_echo end "install packages" success "Finished installing packages on macOS"
}
