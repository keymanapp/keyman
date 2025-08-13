# shellcheck shell=bash
# Keyman is copyright (C) SIL Global. MIT License.

windows_build_action() {
  builder_echo start "build windows" "Building Keyman for Windows"
  # shellcheck disable=SC2154
  "${KEYMAN_ROOT}/windows/build.sh" configure build
  builder_echo end "build windows" success "Finished building Keyman for Windows"
}

windows_test_action() {
  builder_echo start "test windows" "Running Keyman for Windows tests"
  "${KEYMAN_ROOT}/windows/build.sh" test
  builder_echo end "test windows" success "Finished running Keyman for Windows tests"
}

windows_upload_symbols_to_sentry() {
  "${KEYMAN_ROOT}/windows/src/buildtools/sentry-upload-difs.sh"
}
