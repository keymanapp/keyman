# shellcheck shell=bash
# Keyman is copyright (C) SIL Global. MIT License.

android_clean_action() {
  builder_echo start "clean" "Cleaning artifact directories"
  # shellcheck disable=SC2154
  builder_launch /android/build.sh clean
  builder_echo end "clean" success "Finished cleaning artifact directories"
}

android_build_action() {
  builder_echo start "build" "Building Keyman for Android"
  local TARGETS="$1"

  # REVIEW: is it deliberate that we `configure` all targets but only `build,test` `$TARGETS`?
  builder_launch /android/build.sh configure build,test:"${TARGETS}"
  builder_echo end "build" success "Finished building Keyman for Android"
}

android_publish_symbols() {
  local PUBTARGETS="$1"
  builder_echo start "publish to Sentry" "Publishing release to Sentry"

  builder_launch /android/build.sh "publish-symbols:${PUBTARGETS}"

  builder_echo end "publish to Sentry" success "Finished publishing release to Sentry"
}
