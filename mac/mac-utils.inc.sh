
if [ -z "${DEVELOPMENT_TEAM+x}" ]; then
    DEVELOPMENT_TEAM=3YE4W86L3G
fi

function mac_notarize() {
  local TARGET_PATH="$1"
  local TARGET_FILE="$2"

  local NOTARYTOOL_LOG_PATH="$TARGET_PATH/notarytool.log"

  xcrun notarytool submit \
      --apple-id "$APPSTORECONNECT_USERNAME" \
      --team-id "$DEVELOPMENT_TEAM" \
      --password "$APPSTORECONNECT_PASSWORD" \
      --output-format json \
      --wait \
      "$TARGET_FILE" > "$NOTARYTOOL_LOG_PATH"
  # notarytool output: {"status":"Accepted","id":"ca62bba0-6c49-43c2-90d8-83a8ef306e0f","message":"Processing complete"}

  cat "$NOTARYTOOL_LOG_PATH"
  local NOTARYTOOL_STATUS=`cat "$NOTARYTOOL_LOG_PATH" | jq -r .status`
  local NOTARYTOOL_SUBMISSION_ID=`cat "$NOTARYTOOL_LOG_PATH" | jq -r .id`
  if [[ "$NOTARYTOOL_STATUS" != Accepted ]]; then
      # We won't assume notarytool returns an error code if status != Accepted
      builder_die "Notarization failed with $NOTARYTOOL_STATUS"
  fi

  builder_heading "Notarization completed successfully. Review logs below for any warnings."

  xcrun notarytool log \
        --apple-id "$APPSTORECONNECT_USERNAME" \
        --team-id "$DEVELOPMENT_TEAM" \
        --password "$APPSTORECONNECT_PASSWORD" \
        "$NOTARYTOOL_SUBMISSION_ID"

  rm -f "$NOTARYTOOL_LOG_PATH"
}

execCodeSign() {
  # Allow the signing to fail up to 5 times (network transient error on timestamping)
  local ret_code=0
  local count=0
  local method="$1"
  shift
  if [[ "$method" == direct ]]; then
    method=
  fi
  while (( count <  5 )); do
    $method codesign "$@" || ret_code=$?
    if [ $ret_code == 0 ]; then
      return 0
    fi
    (( count++ ))
    builder_echo "codesign attempt $count failed with error $ret_code"
    sleep 5
  done

  builder_echo "*** codesign parameters: $@"
  builder_die "Unable to sign component after 5 attempts (exit code $ret_code)"
}
