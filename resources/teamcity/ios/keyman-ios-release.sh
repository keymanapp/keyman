#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# TC build script for Keyman iOS/Release
#
# Requires: env variable $MAC_BUILDAGENT_PASSWORD set

# shellcheck disable=SC2164
# shellcheck disable=SC1091

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/shellHelperFunctions.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-helpers.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-mac.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/ios/ios-actions.inc.sh"

################################ Main script ################################

builder_describe \
  "Build KeymanEngine and Keyman for iOS" \
  "all            run all actions" \
  "clean          clean artifact directories" \
  "build          build KeymanEngine and Keyman for iOS" \
  "publish        deploy Keyman for iOS" \
  "--sil-itc-team=SIL_ITC_TEAM_ID       SIL Fastlane ITC Team ID to use for TestFlight upload" \
  "--sil-api-key-id=SIL_API_KEY_ID      SIL Fastlane API Key ID to use for TestFlight upload" \
  "--sil-itc-provider=SIL_ITC_PROVIDER  SIL Fastlane ITC Provider to use for TestFlight upload" \
  "--sil-review-info=SIL_REVIEW_INFO    SIL Fastlane Beta App Review Info to use for TestFlight upload" \
  "--fv                                 additionally build FirstVoices app" \
  "--fv-itc-team=FV_ITC_TEAM_ID         FirstVoices Fastlane ITC Team ID to use for TestFlight upload" \
  "--fv-api-key-id=FV_API_KEY_ID        FirstVoices Fastlane API Key ID to use for TestFlight upload" \
  "--fv-itc-provider=FV_ITC_PROVIDER    FirstVoices Fastlane ITC Provider to use for TestFlight upload" \
  "--fv-review-info=FV_REVIEW_INFO      FirstVoices Fastlane Beta App Review Info to use for TestFlight upload" \
  "--rsync-path=RSYNC_PATH              rsync path on remote server" \
  "--rsync-user=RSYNC_USER              rsync user on remote server" \
  "--rsync-host=RSYNC_HOST              rsync host on remote server" \
  "--rsync-root=RSYNC_ROOT              rsync root on remote server" \
  "--help.keyman.com=HELP_KEYMAN_COM    path to help.keyman.com repository"


builder_parse "$@"

cd "${KEYMAN_ROOT}/ios"

function _publish_to_downloads_keyman_com() {
  # Publish to downloads.keyman.com
  builder_echo start "publish to downloads.keyman.com" "Publishing release to downloads.keyman.com"
  (
    local KMEI_DST KEYMANAPP_IPA_DST KEYMAN_CHANGELOG UPLOAD_BASE UPLOAD_FOLDER UPLOAD_DIR

    # Note that the `rsync` tool that exists on Macs by default is a very old version and must be updated to a newer version, e.g. by using Homebrew.
    KMEI_DST="keyman-engine-ios-${KEYMAN_VERSION}.zip"
    KEYMANAPP_IPA_DST="keyman-ios-${KEYMAN_VERSION}.ipa"
    # shellcheck disable=SC2154
    KEYMAN_CHANGELOG="changelog-${KEYMAN_VERSION}-${KEYMAN_TIER}.txt"

    UPLOAD_BASE="upload"
    UPLOAD_FOLDER="${KEYMAN_VERSION}"
    UPLOAD_DIR="${UPLOAD_BASE}/${UPLOAD_FOLDER}"

    # The upload directory has already been created,
    # but the permissions aren't quite in place yet.

    builder_echo "Setting upload file permissions for downloads.keyman.com"
    chmod g+w  "${UPLOAD_DIR}"
    chmod a+rx "${UPLOAD_DIR}"

    chmod g+rw "${UPLOAD_DIR}/${KMEI_DST}" "${UPLOAD_DIR}/${KMEI_DST}.download_info" "${UPLOAD_DIR}/${KEYMANAPP_IPA_DST}" "${UPLOAD_DIR}/${KEYMANAPP_IPA_DST}.download_info" "${UPLOAD_DIR}/${KEYMAN_CHANGELOG}"
    chmod a+r  "${UPLOAD_DIR}/${KMEI_DST}" "${UPLOAD_DIR}/${KMEI_DST}.download_info" "${UPLOAD_DIR}/${KEYMANAPP_IPA_DST}" "${UPLOAD_DIR}/${KEYMANAPP_IPA_DST}.download_info" "${UPLOAD_DIR}/${KEYMAN_CHANGELOG}"

    if builder_has_option --fv; then
      FIRSTVOICESAPP_IPA_DST="firstvoices-ios-${KEYMAN_VERSION}.ipa"
      chmod g+rw "${UPLOAD_DIR}/${FIRSTVOICESAPP_IPA_DST}" "${UPLOAD_DIR}/${FIRSTVOICESAPP_IPA_DST}.download_info"
      chmod a+r  "${UPLOAD_DIR}/${FIRSTVOICESAPP_IPA_DST}" "${UPLOAD_DIR}/${FIRSTVOICESAPP_IPA_DST}.download_info"
    fi

    builder_echo "Performing rsync call"
    cd "${UPLOAD_BASE}"
    tc_rsync_upload "${UPLOAD_FOLDER}" "ios/${KEYMAN_TIER}"
  )
  builder_echo end "publish to downloads.keyman.com" success "Finished publishing release to downloads.keyman.com"
}

function __do_upload_to_appstore() {
  (
    local APP_IPA_DST CHANGELOG_PATH changelog FASTLANE_ITC_TEAM_ID
    local FASTLANE_ITC_PROVIDER="$1"
    local FASTLANE_API_KEY_ID="$2"

    export FASTLANE_ITC_TEAM_ID="$3"
    export LC_ALL=en_US.UTF-8
    export LANG=en_US.UTF-8

    KEYMAN_CHANGELOG="changelog-${KEYMAN_VERSION}-${KEYMAN_TIER}.txt"
    CHANGELOG_PATH="upload/${KEYMAN_VERSION}/${KEYMAN_CHANGELOG}"

    APP_IPA_DST="$4-ios-${KEYMAN_VERSION}.ipa"

    changelog=$(cat "${CHANGELOG_PATH}")

    builder_echo "Uploading to the App Store with the following changelog:"
    builder_echo "${changelog}"
    builder_echo ""

    cd "upload/${KEYMAN_VERSION}"

    # Construct Deliverfile for Fastlane metadata upload.
    cat > Deliverfile << _EOF
release_notes(
  'en-US' => <<-eos
${changelog}
eos
_EOF

    if builder_has_option --fv; then
      cat >> Deliverfile << _EOF
  'en-CA' => <<-eos
${changelog}
eos
)

copyright "#{Time.now.year} SIL Global, First Peoples' Cultural Foundation"
_EOF
    else
      cat >> Deliverfile << _EOF
)

copyright "#{Time.now.year} SIL Global"
_EOF
    fi

    # -f:  Force - skips a visual report Fastlane likes to generate otherwise.  (Apple's standing advice is to skip when doing CI.)
    # shellcheck disable=SC2154
    fastlane deliver \
      --api_key_path "${HOME}/fastlane/${FASTLANE_API_KEY_ID}.json" \
      -f true \
      -u buildagent@keyman.com \
      --submit_for_review true \
      --automatic_release true \
      --precheck_include_in_app_purchases false \
      --verbose \
      --itc_provider "${FASTLANE_ITC_PROVIDER}" \
      --submission_information "{\"add_id_info_uses_idfa\": false}" \
      --ipa "${APP_IPA_DST}"
  )
}

function _publish_to_appstore() {
  builder_echo start "push to AppStore" "Pushing release to AppStore for deployment"
  # shellcheck disable=SC2154
  __do_upload_to_appstore "${SIL_ITC_PROVIDER}" "${SIL_API_KEY_ID}" "${SIL_ITC_TEAM_ID}" keyman
  builder_echo end "push to AppStore" success "Finished pushing release to AppStore for deployment"
}

function _publish_to_appstore_fv() {
  if ! builder_has_option --fv; then
    builder_echo "Skipping FirstVoices upload to AppStore as --fv option is not set"
    return
  fi

  builder_echo start "push FV to AppStore" "Pushing FirstVoices release to AppStore for deployment"
  # shellcheck disable=SC2154
  __do_upload_to_appstore "${FV_ITC_PROVIDER}" "${FV_API_KEY_ID}" "${FV_ITC_TEAM_ID}" firstvoices
  builder_echo end "push FV to AppStore" success "Finished pushing FirstVoices release to AppStore for deployment"
}

function __do_upload_to_fastlane() {
  (
    local APP_IPA_DST PILOT_GROUPS BETA_APP_DESCRIPTION FASTLANE_ITC_TEAM_ID
    local FASTLANE_ITC_PROVIDER="$1"
    local FASTLANE_API_KEY_ID="$2"
    local BETA_APP_REVIEW_INFO="$3"

    export FASTLANE_ITC_TEAM_ID="$4"
    export LC_ALL=en_US.UTF-8
    export LANG=en_US.UTF-8

    KEYMAN_CHANGELOG="changelog-${KEYMAN_VERSION}-${KEYMAN_TIER}.txt"
    CHANGELOG_PATH="upload/${KEYMAN_VERSION}/${KEYMAN_CHANGELOG}"

    APP_IPA_DST="$5-ios-${KEYMAN_VERSION}.ipa"
    BETA_APP_DESCRIPTION="$6"

    changelog=$(cat "${CHANGELOG_PATH}")

    builder_echo "Uploading to Fastlane with the following changelog:"
    builder_echo "${changelog}"
    builder_echo ""

    cd "upload/${KEYMAN_VERSION}"

    # Sets the upload to distribute to the appropriate group of external testers.

    if [[ "${KEYMAN_TIER}" = "beta" ]]; then
      export PILOT_GROUPS="Beta"
    elif [[ "${KEYMAN_TIER}" = "alpha" ]]; then
      export PILOT_GROUPS="Alpha"
    fi

    command -v fastlane
    # shellcheck disable=SC2154
    fastlane pilot upload \
      --api_key_path "${HOME}/fastlane/${FASTLANE_API_KEY_ID}.json" \
      --changelog "${changelog}" \
      -g "${PILOT_GROUPS}" \
      --itc_provider "${FASTLANE_ITC_PROVIDER}" \
      --verbose \
      --ipa "${APP_IPA_DST}" \
      --beta_app_review_info "${BETA_APP_REVIEW_INFO}" \
      --beta_app_description "${BETA_APP_DESCRIPTION}"
  )
}

function _publish_to_fastlane_for_testing() {
  builder_echo start "upload to FastLane" "Uploading build to FastLane for testing-deployment"
    # shellcheck disable=SC2154
  __do_upload_to_fastlane "${SIL_ITC_PROVIDER}" "${SIL_API_KEY_ID}" "${SIL_REVIEW_INFO}" "${SIL_ITC_TEAM_ID}" keyman "Keyman for iPhone and iPad pre-release, see release notes for details"
  builder_echo end "upload to FastLane" success "Finished uploading build to FastLane for testing-deployment"
}

function _publish_to_fastlane_for_testing_fv() {
  if ! builder_has_option --fv; then
    builder_echo "Skipping FirstVoices upload to FastLane as --fv option is not set"
    return
  fi

  builder_echo start "upload to FV FastLane" "Uploading FirstVoices build to FastLane for testing-deployment"
  # shellcheck disable=SC2154
  __do_upload_to_fastlane "${FV_ITC_PROVIDER}" "${FV_API_KEY_ID}" "${FV_REVIEW_INFO}" "${FV_ITC_TEAM_ID}" firstvoices "FirstVoices Keyboards for iPhone and iPad pre-release, see release notes for details"
  builder_echo end "upload to FV FastLane" success "Finished uploading FirstVoices build to FastLane for testing-deployment"
}

function do_build() {
  ios_build
  ios_capture_build_artifacts
}

function do_publish() {
  if builder_has_option --fv; then
    export RELEASE_OEM=true
    export RELEASE_OEM_FIRSTVOICES=true
  else
    export RELEASE_OEM_FIRSTVOICES=false
  fi

  _publish_to_downloads_keyman_com
  upload_help "Keyman for iOS" ios

  if [[ "${KEYMAN_TIER}" == "stable" ]]; then
    _publish_to_appstore
    _publish_to_appstore_fv
  else
    _publish_to_fastlane_for_testing
    _publish_to_fastlane_for_testing_fv
  fi
}

ba_mac_unlock_keychain

if builder_has_action all; then
  ba_mac_clean_xcode_derived_data
  do_build
  do_publish
else
  builder_run_action  clean   ba_mac_clean_xcode_derived_data
  builder_run_action  build   do_build
  builder_run_action  publish do_publish
fi
