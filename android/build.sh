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
. "${THIS_SCRIPT%/*}/../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"
. "$KEYMAN_ROOT/resources/build/zip.inc.sh"

################################ Main script ################################

#
# Restrict available OEM publish targets to those that can be built on the current system
#

oemtargets=()
if [ ! -z "${RELEASE_OEM+x}" ]; then
  if [ "${RELEASE_OEM_FIRSTVOICES-false}" = true ]; then
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
  "publish                                  Publishes symbols to Sentry and the APKs to the Play Store." \
  "archive                                  Copy release artifacts to upload/ and rsync to downloads.keyman" \
  "--ci+                                    Deprecated build option. Remove in 20.0" \
  --upload-sentry+ \
  ":engine=KMEA                             Keyman Engine for Android" \
  ":app=KMAPro                              Keyman for Android" \
  ":help                                    Online documentation" \
  ":sample1=Samples/KMSample1               Sample app: KMSample1" \
  ":sample2=Samples/KMSample2               Sample app: KMSample2" \
  ":keyboardharness=Tests/KeyboardHarness   Test app: KeyboardHarness" \
  "${oemtargets[@]}" \

builder_parse "$@"

# Override JAVA_HOME to OpenJDK 11
set_java_home

# This script also responsible for cleaning up /android/upload
builder_run_child_actions clean

if builder_start_action clean; then
  builder_heading "Cleanup /android/upload"
  rm -rf "$KEYMAN_ROOT/android/upload"
  builder_finish_action success clean
fi

builder_run_child_actions configure build test

function do_test_help() {
  check-markdown  "$KEYMAN_ROOT/android/docs/help"
  check-markdown  "$KEYMAN_ROOT/android/docs/engine"
}

builder_run_action        test:help    do_test_help

builder_run_child_actions publish

# Copy release artifacts to upload/ and rsync to downloads.keyman.com
if builder_start_action archive; then
  UPLOAD_PATH="$KEYMAN_ROOT/android/upload/${KEYMAN_VERSION}"
  KEYMAN_ENGINE_ANDROID_ZIP="keyman-engine-android-${KEYMAN_VERSION}.zip"
  KEYMAN_APK="keyman-${KEYMAN_VERSION}.apk"
  FIRSTVOICES_APK="firstvoices-${KEYMAN_VERSION}.apk"

  mkdir -p "${UPLOAD_PATH}"

  # Create Keyman Engine for Android archive
  ZIP_FILE="${UPLOAD_PATH}/${KEYMAN_ENGINE_ANDROID_ZIP}"
  ZIP_FLAGS=("-q" "-r") # quiet, recursive

  builder_echo "Copying Keyman Engine for Android into ${UPLOAD_PATH}..."
  cd "${UPLOAD_PATH}"
  cp "${KEYMAN_ROOT}/android/KMAPro/kMAPro/libs/keyman-engine.aar" ./
  add_zip_files "${ZIP_FILE}" "${ZIP_FLAGS[@]}" "keyman-engine.aar"
  rm -f "keyman-engine.aar"

  builder_echo "Copying Keyman Engine for Android Sample projects into ${UPLOAD_PATH}..."
  cp -rf "${KEYMAN_ROOT}/android/Samples" ./
  add_zip_files "${ZIP_FILE}" "-x@../../zip-excludes" "${ZIP_FLAGS[@]}" "Samples"
  rm -rf "Samples"

  # Copy release APK
  cp "${KEYMAN_ROOT}/android/KMAPro/kMAPro/build/outputs/apk/release/${KEYMAN_APK}" \
    "${UPLOAD_PATH}/${KEYMAN_APK}"

  # FirstVoices app

  if [ "${RELEASE_OEM_FIRSTVOICES-false}" = true ]; then
    cp "${KEYMAN_ROOT}/oem/firstvoices/android/app/build/outputs/apk/release/${FIRSTVOICES_APK}" \
      "${UPLOAD_PATH}/${FIRSTVOICES_APK}"
  fi

  #
  # Write download info files
  #

  cd "${UPLOAD_PATH}"
  write_download_info "Keyman Engine for Android" "${KEYMAN_ENGINE_ANDROID_ZIP}" "${KEYMAN_VERSION}" "${KEYMAN_TIER}" "android"
  write_download_info "Keyman for Android" "${KEYMAN_APK}" "${KEYMAN_VERSION}" "${KEYMAN_TIER}" "android"

  if [ "${RELEASE_OEM_FIRSTVOICES-false}" = true ]; then
    write_download_info "FirstVoices Keyboards" "${FIRSTVOICES_APK}" "${KEYMAN_VERSION}" "${KEYMAN_TIER}" "android"
  fi

  builder_finish_action success archive
fi
