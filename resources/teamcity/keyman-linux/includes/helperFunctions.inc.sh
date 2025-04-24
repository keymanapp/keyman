#!/bin/bash

COLOR_GREEN='\x1b[32m'              # $(tput setaf 2)
COLOR_RESET='\x1b(B\x1b[m'          # $(tput sgr0)

is_os_version_or_higher() {
  OS_VERSION=$1

  # we use `dpkg --compare-versions` to compare the current Ubuntu version
  dpkg --compare-versions "$(lsb_release -r -s)" ge "${OS_VERSION}"
}

is_package_installed() {
  PACKAGE=$1

  dpkg -l "${PACKAGE}" >/dev/null 2>&1
}

log() {
  echo -e "${COLOR_GREEN}$1${COLOR_RESET}"
}
