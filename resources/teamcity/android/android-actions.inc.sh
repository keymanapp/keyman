# shellcheck shell=bash
# Keyman is copyright (C) SIL Global. MIT License.

android_clean_action() {
  builder_echo start "clean" "Cleaning artifact directories"
  # shellcheck disable=SC2154
  "${KEYMAN_ROOT}/android/build.sh" clean
  builder_echo end "clean" success "Finished cleaning artifact directories"
}

android_build_action() {
  builder_echo start "build" "Building Keyman for Android"
  local TARGETS="$1"

  # REVIEW: is it deliberate that we `configure` all targets but only `build,test` `$TARGETS`?
  "${KEYMAN_ROOT}/android/build.sh" configure build,test:"${TARGETS}" --debug
  builder_echo end "build" success "Finished building Keyman for Android"
}
