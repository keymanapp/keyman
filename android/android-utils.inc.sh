# shellcheck shell=bash
# Keyman is copyright (C) SIL Global. MIT License.

function android_set_gradle_environment() {
  if builder_is_debug_build; then
    GRADLE_TARGET=assembleDebug
    ARCHIVE_TARGET=debug
  else
    GRADLE_TARGET=build
    ARCHIVE_TARGET=release-unsigned
  fi

  if builder_is_ci_build; then
    GRADLE_DAEMON=-no-daemon
  else
    GRADLE_DAEMON=
  fi

  export GRADLE_TARGET GRADLE_DAEMON ARCHIVE_TARGET
}