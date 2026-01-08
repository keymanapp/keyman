#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# TC build script for Keyman iOS/Test
#
# Requires: env variable $MAC_BUILDAGENT_PASSWORD set

# shellcheck disable=SC2164
# shellcheck disable=SC1091

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/build/utils.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-mac.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/ios/ios-actions.inc.sh"

################################ Main script ################################

# TODO: remove unused `--pr-number` option. Unfortunately, the mechanism we
# designed to skip this, `--builder-ignore-unknown-options`, does not work for
# options with values. We need another one, for example, something like
# `--builder-ignore-unknown-value-options=pr-number[,...]`, so for now I will
# leave it alone. The option ultimately needs to be removed from the TC build
# configuration also. (Unused as of #14466)

builder_describe \
  "Build KeymanEngine and Keyman for iOS" \
  "all            run all actions" \
  "clean          clean artifact directories" \
  "build          build KeymanEngine and Keyman for iOS" \
  "--pr-number=PR_NUMBER                Currently building PR" \
  "--sil-itc-team=SIL_ITC_TEAM_ID       SIL Fastlane ITC Team ID to use for TestFlight upload" \
  "--sil-api-key-id=SIL_API_KEY_ID      SIL Fastlane API Key ID to use for TestFlight upload" \
  "--sil-itc-provider=SIL_ITC_PROVIDER  SIL Fastlane ITC Provider to use for TestFlight upload" \
  "--fv                                 additionally build FirstVoices app" \
  "--fv-itc-team=FV_ITC_TEAM_ID         FirstVoices Fastlane ITC Team ID to use for TestFlight upload" \
  "--fv-api-key-id=FV_API_KEY_ID        FirstVoices Fastlane API Key ID to use for TestFlight upload" \
  "--fv-itc-provider=FV_ITC_PROVIDER    FirstVoices Fastlane ITC Provider to use for TestFlight upload"


builder_parse "$@"

cd "${KEYMAN_ROOT}/ios"

function __do_upload_to_testflight() {
  (
    local FASTLANE_ITC_PROVIDER="$1"
    local FASTLANE_API_KEY_ID="$2"
    local APP_IPA_DST

    export FASTLANE_ITC_TEAM_ID="$3"
    export LC_ALL=en_US.UTF-8
    export LANG=en_US.UTF-8

    APP_IPA_DST="$4-ios-${KEYMAN_VERSION_FOR_FILENAME}.ipa"

    cd "upload/${KEYMAN_VERSION}"
    command -v fastlane
    # shellcheck disable=SC2154
    fastlane pilot upload \
      --distribute_external false \
      --notify_external_testers false \
      --submit_beta_review false \
      --api_key_path "${HOME}/fastlane/${FASTLANE_API_KEY_ID}.json" \
      -g "PR Builds" \
      --itc_provider "${FASTLANE_ITC_PROVIDER}" \
      --verbose \
      --ipa "${APP_IPA_DST}"
  )
}

function _upload_to_testflight_pr_area() {
  builder_echo start "upload to testflight" "Uploading build to TestFlight (PR area)"
  # shellcheck disable=SC2154
  __do_upload_to_testflight "${SIL_ITC_PROVIDER}" "${SIL_API_KEY_ID}" "${SIL_ITC_TEAM_ID}"  keyman
  builder_echo end "upload to testflight" success "Finished uploading build to TestFlight (PR area)"
}

function _upload_to_testflight_pr_area_fv() {
  if ! builder_has_option --fv; then
    builder_echo "Skipping FirstVoices upload to TestFlight as --fv option is not set"
    return
  fi

  builder_echo start "upload to testflight" "Uploading FirstVoices build to TestFlight (PR area) - FirstVoices"
  # shellcheck disable=SC2154
  __do_upload_to_testflight "${FV_ITC_PROVIDER}" "${FV_API_KEY_ID}" "${FV_ITC_TEAM_ID}"  firstvoices
  builder_echo end "upload to testflight" success "Finished uploading FirstVoices build to TestFlight (PR area) - FirstVoices"
}

function do_build() {
  ios_build
  ios_capture_build_artifacts
  builder_if_release_build_level _upload_to_testflight_pr_area
  builder_if_release_build_level _upload_to_testflight_pr_area_fv
}

ba_mac_unlock_keychain

if builder_has_action all; then
  ba_mac_clean_xcode_derived_data
  do_build
else
  builder_run_action  clean   ba_mac_clean_xcode_derived_data
  builder_run_action  build   do_build
fi
