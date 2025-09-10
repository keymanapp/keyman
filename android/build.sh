#!/usr/bin/env bash
#
# Build Keyman Engine for Android, Keyman for Android, OEM FirstVoices Android app,
# Samples: KMsample1 and KMSample2, Test - KeyboardHarness
#
# OEM FirstVoices Android app requires the following env vars set:
# RELEASE_OEM should be set
# RELEASE_OEM_FIRSTVOICES should be true

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/build/utils.inc.sh"
. "${KEYMAN_ROOT}/resources/build/zip.inc.sh"

################################ Main script ################################

#
# Restrict available OEM publish targets to those that can be built on the current system
#

oemtargets=()
if [[ ! -z "${RELEASE_OEM+x}" ]]; then
  if [[ "${RELEASE_OEM_FIRSTVOICES-false}" = true ]]; then
    oemtargets+=(":fv=../oem/firstvoices/android           OEM FirstVoices for Android app")
  fi
fi

builder_describe \
  "Build Keyman Engine for Android, Keyman for Android, and FirstVoices Android app." \
  "@/resources/tools/check-markdown  test:help" \
  clean \
  configure \
  build \
  test \
  "publish-symbols                          Publishes symbols to Sentry" \
  "publish-play-store                       Publishes APKs to the Play Store" \
  "archive                                  Copy release artifacts to upload/ and create zip" \
  ":engine=KMEA                             Keyman Engine for Android" \
  ":app=KMAPro                              Keyman for Android" \
  ":help                                    Online documentation" \
  ":sample1=Samples/KMSample1               Sample app: KMSample1" \
  ":sample2=Samples/KMSample2               Sample app: KMSample2" \
  ":keyboardharness=Tests/KeyboardHarness   Test app: KeyboardHarness" \
  "${oemtargets[@]}" \

builder_parse "$@"

function do_clean() {
  builder_heading "Cleanup /android/upload"
  rm -rf "${KEYMAN_ROOT}/android/upload"
}

function do_test_help() {
  check-markdown  "${KEYMAN_ROOT}/android/docs/help"
  check-markdown  "${KEYMAN_ROOT}/android/docs/engine"
}

function archive_artifacts() {
  local UPLOAD_PATH KEYMAN_ENGINE_ANDROID_ZIP KEYMAN_APK FIRSTVOICES_APK
  local KEYMAN_FULLY_VERSIONED_APK FIRSTVOICES_FULLY_VERSIONED_APK

  UPLOAD_PATH="${KEYMAN_ROOT}/android/upload/${KEYMAN_VERSION}"
  KEYMAN_ENGINE_ANDROID_ZIP="keyman-engine-android-${KEYMAN_VERSION}.zip"
  KEYMAN_APK="keyman-${KEYMAN_VERSION_FOR_FILENAME}.apk"
  FIRSTVOICES_APK="firstvoices-${KEYMAN_VERSION_FOR_FILENAME}.apk"

  rm -rf "${UPLOAD_PATH}"
  mkdir -p "${UPLOAD_PATH}"

  # Create Keyman Engine for Android archive
  builder_echo "Adding Keyman Engine for Android to zip..."
  (
    # shellcheck disable=SC2164
    cd "${KEYMAN_ROOT}/android/KMAPro/kMAPro/libs"
    add_zip_files -q -r "${UPLOAD_PATH}/${KEYMAN_ENGINE_ANDROID_ZIP}" keyman-engine.aar
  )

  builder_echo "Adding Keyman Engine for Android Sample projects to zip..."
  (
    # shellcheck disable=SC2164
    cd "${KEYMAN_ROOT}/android"
    add_zip_files -q -r -xr!build.sh "${UPLOAD_PATH}/${KEYMAN_ENGINE_ANDROID_ZIP}" Samples
  )

  # Copy release APK
  cp "${KEYMAN_ROOT}/android/KMAPro/kMAPro/build/outputs/apk/release/${KEYMAN_APK}" "${UPLOAD_PATH}/${KEYMAN_APK}"

  # FirstVoices app

  if [[ "${RELEASE_OEM_FIRSTVOICES-false}" = true ]]; then
    cp "${KEYMAN_ROOT}/oem/firstvoices/android/app/build/outputs/apk/release/${FIRSTVOICES_APK}" "${UPLOAD_PATH}/${FIRSTVOICES_APK}"
  fi

  #
  # Write download info files
  #

  write_download_info "${UPLOAD_PATH}" "${KEYMAN_ENGINE_ANDROID_ZIP}" "Keyman Engine for Android" zip android
  write_download_info "${UPLOAD_PATH}" "${KEYMAN_APK}" "Keyman for Android" apk android

  if [[ "${RELEASE_OEM_FIRSTVOICES-false}" = true ]]; then
    write_download_info "${UPLOAD_PATH}" "${FIRSTVOICES_APK}" "FirstVoices Keyboards" apk android
  fi
}


# For CI compatibility of building Keyman for Android 18.0 with OpenJDK 11,
# this overrides JAVA_HOME for the builder script to use OpenJDK 21.
android_set_java_home() {
  if [[ ! -z ${JAVA_HOME_21+x} ]]; then
    builder_echo "Setting JAVA_HOME to JAVA_HOME_21 (${JAVA_HOME_21})"
    export JAVA_HOME="${JAVA_HOME_21}"
  fi
}

# Override JAVA_HOME
android_set_java_home

# This script also responsible for cleaning up /android/upload
builder_run_child_actions clean

builder_run_action        clean     do_clean

builder_run_child_actions configure build test

builder_run_action        test:help  do_test_help

builder_run_child_actions publish-symbols publish-play-store

builder_run_action        archive    archive_artifacts
