#!/usr/bin/env bash

#
# This script checks the built, minified file size for keymanweb.js against
# the latest published version and optionally issues a status check against
# the relevant commit on GitHub reporting on the file size and its suitability.
#

set -eu

# If keymanweb.js is more than SIZE_THRESHOLD bytes larger, we emit a failure status
SIZE_THRESHOLD=1024

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/build-utils.sh"
. "$KEYMAN_ROOT/resources/build/jq.inc.sh"
. "$KEYMAN_ROOT/resources/build/github.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

display_usage() {
  echo "check-build-size.sh [--write-status|-w] [--report|-r] [--sha sha] [--github-token token]"
  echo
  echo "Checks the built file size for KeymanWeb against the most recent"
  echo "successful build."
  echo
  echo "Reports the file size to console, and optionally adds a status check"
  echo "to the repository on GitHub, if --write-status|-w is set. The env vars"
  echo "GITHUB_TOKEN and BUILD_VCS_NUMBER are used if present."
  echo
  echo "If --report is not set, the script will exit with code 0 if there are no"
  echo "errors, even if the file size is larger."
  echo
  echo "Parameters:"
  echo "  --write-status, -w      Writes a status check to TeamCity"
  echo "  --report, -r            Set exit code to:"
  echo "                            0, success, file size <= current version"
  echo "                            1, warning, file size slightly larger"
  echo "                            2, failure, file size significantly larger"
  echo "                            3, error, script failed"
  echo "  --github-token token    Use token to report to GitHub, defaults to \$GITHUB_TOKEN"
  echo "  --sha sha               Use sha for status check, defaults to \$BUILD_VCS_NUMBER"
  exit 1
}

parse_params() {
  while [[ $# -gt 0 ]] ; do
    key="$1"
    case $key in
      --write-status|-w)
        WRITE_STATUS=true
        ;;
      --report|-r)
        REPORT=true
        ;;
      --sha)
        TARGET_SHA="$2"
        shift
        ;;
      --github-token)
        GITHUB_TOKEN="$2"
        shift
        ;;
      --help|-h|-\?)
        display_usage
        exit 0
        ;;
      *)
        echo "$0: invalid option: $key"
        display_usage
        exit 64
    esac
    shift # past argument
  done
}

WRITE_STATUS=false
REPORT=false

parse_params "$@"

#
# Get file size of the latest local minified build
#

LOCAL_FILE=web/build/publish/release/keymanweb.js
LOCAL_FILE_SIZE=`stat --printf="%s" $KEYMAN_ROOT/$LOCAL_FILE`

#
# Get the most recent build for $TIER from downloads.keyman.com
#

DOWNLOADS_VERSION_API=https://downloads.keyman.com/api/version/web
REMOTE_KEYMANWEB_VERSIONS=`curl -s $DOWNLOADS_VERSION_API`
REMOTE_VERSION=`echo $REMOTE_KEYMANWEB_VERSIONS | $JQ -r ".web.$TIER"`

REMOTE_FILE_NEW=https://downloads.keyman.com/web/$TIER/$REMOTE_VERSION/static/build/app/browser/release/keymanweb.js
REMOTE_FILE_OLD=https://downloads.keyman.com/web/$TIER/$REMOTE_VERSION/static/build/app/web/release/keymanweb.js

# If the remote file does not exist at the 'new' location, try the 'old' one instead.
# Allows reorganization 'fallback' for the file-size check.
REMOTE_NEW_EXISTS=`curl "$REMOTE_FILE_NEW" --location --silent --write-out '%{http_code}' --output /dev/null`

if [ $REMOTE_NEW_EXISTS == "404" ]; then
  # Fallback to "old" URL!
  echo "⚠️ Falling back to prior previous-version URL structure."
  REMOTE_FILE_SIZE=`curl "$REMOTE_FILE_OLD" --location --silent --write-out '%{size_download}' --output /dev/null`
else
  REMOTE_FILE_SIZE=`curl "$REMOTE_FILE_NEW" --location --silent --write-out '%{size_download}' --output /dev/null`

fi

#
# Report statistics
#

echo "This build of keymanweb.js: $(printf "%'d" $LOCAL_FILE_SIZE) bytes"
echo "Last release build of keymanweb.js, version $REMOTE_VERSION: $(printf "%'d" $REMOTE_FILE_SIZE) bytes"

#
# Verify that we are within expected bounds
#

DIFF=$(($REMOTE_FILE_SIZE - $LOCAL_FILE_SIZE))
if (( $DIFF < 0 )); then
  DIFF=$((-$DIFF))
fi

PERCENT_DIFF=$((DIFF * 100 / $REMOTE_FILE_SIZE))

if (( $LOCAL_FILE_SIZE <= $REMOTE_FILE_SIZE )); then
  # We are good -- the new build is smaller than the old one
  RESULT_STATE=success
  REPORT_CODE=0
  RESULT_MESSAGE="✅ Excellent! keymanweb.js is $(printf "%'d" $DIFF) bytes ($PERCENT_DIFF%) smaller than $REMOTE_VERSION, now $(printf "%'d" $LOCAL_FILE_SIZE) bytes"
elif (( $DIFF < $SIZE_THRESHOLD )); then
  # Warning, larger, but less than 1kb larger
  RESULT_STATE=success
  REPORT_CODE=1
  RESULT_MESSAGE="⚠️ Warning: keymanweb.js is $(printf "%'d" $DIFF) bytes ($PERCENT_DIFF%) larger than $REMOTE_VERSION, now $(printf "%'d" $LOCAL_FILE_SIZE) bytes"
else
  # Failure - new build is significantly larger
  DIFF=$(($LOCAL_FILE_SIZE - $REMOTE_FILE_SIZE))
  RESULT_STATE=failure
  REPORT_CODE=2
  RESULT_MESSAGE="❌ Oh dear! keymanweb.js is $(printf "%'d" $DIFF) bytes ($PERCENT_DIFF%) larger than $REMOTE_VERSION, now $(printf "%'d" $LOCAL_FILE_SIZE) bytes"
fi

echo "$RESULT_MESSAGE"

if $WRITE_STATUS; then
  if [ -z ${GITHUB_TOKEN+x} ]; then
    # If we have a GITHUB_TOKEN env var set, either via environment in TeamCity,
    # or via command line parameter, then we can attempt to report a GitHub status
    # check
    builder_die "Error: GITHUB_TOKEN variable is not set."
  fi

  if [ ! -z ${BUILD_VCS_NUMBER+x} ]; then
    # If we are running on TeamCity, we should have the BUILD_VCS_NUMBER env var
    # referencing the git commit sha against which we should report the status
    write_github_status_check_from_teamcity "check/web/file-size" "$RESULT_STATE" "$RESULT_MESSAGE" ""
  elif [ ! -z ${TARGET_SHA+x} ]; then
    # When running locally, we can report to GitHub if the sha is passed on the
    # command line
    write_github_status_check "check/web/file-size" "$RESULT_STATE" "$RESULT_MESSAGE" "" $TARGET_SHA
  else
    builder_die "Error: could not find SHA to report against."
  fi
fi

if $REPORT; then
  exit $REPORT_CODE
fi

exit 0
