#!/usr/bin/env bash
# TC build script for Keyman Linux/Test
# shellcheck disable=SC2164
# shellcheck disable=SC1091

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

builder_describe \
  "Tests install build, making a source package and runs unit tests." \
  "configure      install dependencies" \
  "build          make a release build" \
  "test           run unit tests" \
  "publish        make a source tarball"

builder_parse "$@"

cd "${KEYMAN_ROOT}/linux"

function additional_dependencies_action() {
  builder_echo start additional_dependencies "Installing additional dependencies"
  TOINSTALL=""
  for p in lcov libdatetime-perl gcovr python3-venv jq
  do
    if ! is_package_installed "${p}"; then
      TOINSTALL="${TOINSTALL} ${p}"
    fi
  done

  if is_os_version_or_higher 24.04; then
    for p in libgirepository-2.0-dev python3-coverage
    do
      if ! is_package_installed "${p}"; then
        TOINSTALL="${TOINSTALL} ${p}"
      fi
    done
  fi

  if [[ -n "${TOINSTALL}" ]]; then
    sudo apt-get update
    # shellcheck disable=SC2086
    sudo DEBIAN_FRONTEND="noninteractive" apt-get install -qy ${TOINSTALL}
  fi

  if ! is_os_version_or_higher 24.04; then
    builder_heading "Installing python3-coverage from pip"
    pip3 install --user coverage
  fi

  builder_heading "Checking and installing nvm"
  if [[ ! -f "${HOME}/.nvm/nvm.sh" ]]; then
    # Install nvm
    # shellcheck disable=SC2312
    NVM_RELEASE=$(curl -s https://api.github.com/repos/nvm-sh/nvm/releases/latest | grep tag_name | cut -d : -f 2 | cut -d '"' -f 2)
    # shellcheck disable=SC2312
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/${NVM_RELEASE}/install.sh | bash
    set_variables_for_nvm
    nvm install --lts --default --save
    nvm use --lts
  fi
  builder_echo end additional_dependencies success "Installing additional dependencies"
}

function make_source_tarball_action() {
  builder_echo start "make source tarball" "Make source tarball"
  rm -rf dist
  make tmpsources
  builder_echo end "make source tarball" success "Make source tarball"
}

builder_run_action  configure   linux_install_dependencies_action
builder_run_action  configure   additional_dependencies_action

set_variables_for_nvm

builder_run_action  build       linux_build_action --coverage
builder_run_action  test        linux_unit_tests_action --coverage --report --no-integration
builder_run_action  publish     make_source_tarball_action
