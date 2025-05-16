#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

################################ Main script ################################

builder_describe \
  "Build keyman-config." \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "install                   install artifacts" \
  "uninstall                 uninstall artifacts" \
  "--no-integration          don't run integration tests" \
  "--report                  create coverage report" \
  "--coverage                capture test coverage"

builder_parse "$@"

builder_describe_outputs \
  build "/linux/keyman-config/keyman_config/standards/lang_tags_map.py"

clean_action() {
  rm -rf dist make_deb build ./*.egg-info keyman_config/version.py
  find . \( -name __pycache__ -o -name keyman-config.mo \) -exec rm -rf {} +
  rm -rf ../docs/help/reference/km-*.md

  # Don't delete this file during a package build because they are
  # part of the source package. We can't generate it during a package
  # build because we need to get data from the network which isn't
  # available for package builds.
  if [ -z "${KEYMAN_PKG_BUILD-}" ]; then
    rm -rf keyman_config/standards/lang_tags_map.py
  fi
}

execute_with_temp_schema() {
  local TEMP_DATA_DIR SCHEMA_DIR
  TEMP_DATA_DIR=$(mktemp -d)
  SCHEMA_DIR="${TEMP_DATA_DIR}/glib-2.0/schemas"
  export XDG_DATA_DIRS="${TEMP_DATA_DIR}":${XDG_DATA_DIRS-}
  export GSETTINGS_SCHEMA_DIR="${SCHEMA_DIR}:/usr/share/glib-2.0/schemas/:${GSETTINGS_SCHEMA_DIR-}"
  mkdir -p "${SCHEMA_DIR}"
  cp resources/com.keyman.gschema.xml "${SCHEMA_DIR}"/
  glib-compile-schemas "${SCHEMA_DIR}"
  "$@"
  export XDG_DATA_DIRS=${XDG_DATA_DIRS#*:}
  unset GSETTINGS_SCHEMA_DIR
  rm -rf "${TEMP_DATA_DIR}"
}

build_man_and_help_pages() {
  execute_with_temp_schema ./build-help.sh --no-reconf
}

build_action() {
  builder_echo "Create version.py"
  pushd keyman_config
  sed \
      -e "s/_KEYMAN_VERSION_/${KEYMAN_VERSION}/g" \
      -e "s/_KEYMAN_VERSION_WITH_TAG_/${KEYMAN_VERSION_WITH_TAG}/g" \
      -e "s/_KEYMAN_VERSION_GIT_TAG_/${KEYMAN_VERSION_GIT_TAG}/g" \
      -e "s/_KEYMAN_VERSION_MAJOR_/${KEYMAN_VERSION_MAJOR}/g" \
      -e "s/_KEYMAN_VERSION_RELEASE_/${KEYMAN_VERSION_RELEASE}/g" \
      -e "s/_KEYMAN_TIER_/${KEYMAN_TIER}/g" \
      -e "s/_KEYMAN_VERSION_ENVIRONMENT_/${KEYMAN_VERSION_ENVIRONMENT}/g" \
      -e "s/_UPLOAD_SENTRY_/${UPLOAD_SENTRY}/g" \
      version.py.in > version.py
  popd
  pushd buildtools
  if [ -f build-langtags.py ]; then
    builder_echo "Create lang_tags_map.py"
    python3 ./build-langtags.py
  else
    builder_echo "Skip building lang_tags_map.py during package build"
  fi
  popd
  builder_echo "Building man and help pages"
  build_man_and_help_pages
  builder_echo "Building keyman-config"
  # we use `dpkg --compare-versions` to compare the current Ubuntu version
  if dpkg --compare-versions "$(lsb_release -r -s)" lt 24.04; then
    # Ubuntu 22.04 Jammy has a buggy version of python3-build which doesn't work
    # TODO: remove once we drop support for Ubuntu 22.04 Jammy
    python3 setup.py build
  else
    python3 -m build --outdir build .
  fi
}

test_action() {
  local options

  if builder_has_option --coverage; then
    options="--coverage"
  else
    options=""
  fi
  execute_with_temp_schema ./run-tests.sh "${options}"

  if builder_has_option --report; then
    builder_echo "Creating coverage report"
    python3 -m coverage html --directory="$THIS_SCRIPT_PATH/build/coveragereport/" --data-file=build/.coverage
  fi
}

install_action() {
  if [ -n "${SUDO_USER:-}" ]; then
    # with sudo install into /usr/local
    pip3 install qrcode sentry-sdk
    pip3 install .
    # install icons
    mkdir -p /usr/local/share/keyman/icons
    cp keyman_config/icons/* /usr/local/share/keyman/icons
    # install man pages
    mkdir -p /usr/local/share/man/man1
    cp ../../debian/man/*.1 /usr/local/share/man/man1
  else
    # without sudo install into /tmp/keyman (or $DESTDIR)
    mkdir -p "/tmp/keyman/$(python3 -c 'import sys;import os;pythonver="python%d.%d" % (sys.version_info[0], sys.version_info[1]);sitedir = os.path.join("lib", pythonver, "site-packages");print(sitedir)')"
    pip3 install --prefix /tmp/keyman .
  fi
}

uninstall_action() {
  # run as sudo
  clean_action
  rm -rf /usr/local/share/keyman/icons
  rm -f /usr/local/share/man/man1/km-*.1
  pip3 uninstall keyman_config
  rm -f /usr/local/bin/km-config
  rm -f /usr/local/bin/km-kvk2ldml
  rm -f /usr/local/bin/km-package-get
  rm -f /usr/local/bin/km-package-install
  rm -f /usr/local/bin/km-package-list-installed
  rm -f /usr/local/bin/km-package-uninstall
}

builder_run_action clean      clean_action
builder_run_action configure  # nothing to do
builder_run_action build      build_action
builder_run_action test       test_action
builder_run_action install    install_action
builder_run_action uninstall  uninstall_action
