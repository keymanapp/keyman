#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
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

builder_parse "$@"

cd "$THIS_SCRIPT_PATH"

builder_describe_outputs \
  build "/linux/keyman-config/keyman_config/standards/lang_tags_map.py"

clean_action() {
  rm -rf dist make_deb build keyman_config/version.py ./*.egg-info __pycache__ \
    keyman_config/standards/lang_tags_map.py
}

build_action() {
  builder_echo "Create version.py"
  pushd keyman_config
  sed \
      -e "s/_VERSION_/${VERSION}/g" \
      -e "s/_VERSIONWITHTAG_/${VERSION_WITH_TAG}/g" \
      -e "s/_VERSIONGITTAG_/${VERSION_GIT_TAG}/g" \
      -e "s/_MAJORVERSION_/${VERSION_MAJOR}/g" \
      -e "s/_RELEASEVERSION_/${VERSION_RELEASE}/g" \
      -e "s/_TIER_/${TIER}/g" \
      -e "s/_ENVIRONMENT_/${VERSION_ENVIRONMENT}/g" \
      -e "s/_UPLOADSENTRY_/${UPLOAD_SENTRY}/g" \
      version.py.in > version.py
  popd
  pushd buildtools
  builder_echo "Create lang_tags_map.py"
  python3 ./build-langtags.py
  popd
  builder_echo "Building man pages"
  ./build-help.sh --man --no-reconf
  builder_echo "Building keyman-config"
  python3 setup.py build
}

install_action() {
  if [ -n "${SUDO_USER:-}" ]; then
    pip3 install qrcode sentry-sdk
    # eventually change this to: pip3 install .
    python3 setup.py install
    # install icons
    mkdir -p /usr/local/share/keyman/icons
    cp keyman_config/icons/* /usr/local/share/keyman/icons
    # install man pages
    mkdir -p /usr/local/share/man/man1
    cp ../../debian/man/*.1 /usr/local/share/man/man1
  else
    mkdir -p "/tmp/keyman/$(python3 -c 'import sys;import os;pythonver="python%d.%d" % (sys.version_info[0], sys.version_info[1]);sitedir = os.path.join("lib", pythonver, "site-packages");print(sitedir)')"
    # when we no longer have to support old pip version (python > 3.6) change this to:
    # pip3 install --prefix /tmp/keyman .
    PYTHONUSERBASE=/tmp/keyman python3 setup.py install --user
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
builder_run_action test       ./run-tests.sh
builder_run_action install    install_action
builder_run_action uninstall  uninstall_action
