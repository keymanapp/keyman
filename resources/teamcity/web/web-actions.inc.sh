# shellcheck shell=bash
# Keyman is copyright (C) SIL Global. MIT License.

web_install_dependencies_on_linux_action() {
  if ! builder_is_linux; then
    return 0
  fi

  builder_echo start "install dependencies" "Install dependencies"

  # shellcheck disable=SC2086
  ba_linux_check_and_install_packages devscripts jq

  ba_linux_install_nvm
  _install_playwright_dependencies

  builder_echo end "install dependencies" success "Finished installing dependencies"
}

_install_playwright_dependencies() {
  if ! builder_is_linux || ! ba_linux_is_os_version_or_higher 24.04; then
    return 0
  fi

  # shellcheck disable=SC2086
  ba_linux_check_and_install_packages ibevent-2.1-7t64 libxslt1.1 libwoff1 \
    libvpx9 libgstreamer-plugins-bad1.0-0 libwebpdemux2 libharfbuzz-icu0 \
    libenchant-2-2 libsecret-1-0 libhyphen0 libmanette-0.2-0 libflite1 \
    gstreamer1.0-libav libnss3 libnspr4 libatk1.0-0t64 libatk-bridge2.0-0t64 \
    libcups2t64 libatspi2.0-0t64 libxcomposite1 libxdamage1 libxrandr2 \
    libxcursor1 libgtk-3-0t64 libgles2
}

web_build_action() {
  builder_echo start web_build "Building web"
  # shellcheck disable=SC2154
  "${KEYMAN_ROOT}/web/ci.sh" build
  builder_echo end web_build success "Finished building web"
}

web_test_action() {
  builder_echo start web_test "Running tests for native KeymanWeb"
  if builder_is_linux; then
    ba_linux_start_xvfb
    trap "ba_linux_stop_xvfb" ERR
  fi

  "${KEYMAN_ROOT}/web/ci.sh" test

  if builder_is_linux; then
    ba_linux_stop_xvfb
    trap ERR
  fi

  builder_launch /web/build.sh coverage

  builder_echo end web_test success "Finished running tests for native KeymanWeb"
}

