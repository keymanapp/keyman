#!/usr/bin/env bash

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../build-utils.sh"
# END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

##############

CHECK="${COLOR_GREEN}\xE2\x9C\x94${COLOR_RESET}" # ✔
CROSS="${COLOR_RED}\xE2\x9D\x8C${COLOR_RESET}" # ❌

builder_describe \
  "Provides actions useful for build-utils.sh trap-functionality unit testing.\nOnly one action may be set at a time." \
  "error" \
  "incomplete"

builder_parse "$@"

###

HR="------------------------------------------------"

error-test-matcher() {
  ERR_CODE=$?
  ALL_PASS=true

  echo "$HR"

  if [[ $ERR_CODE != 0 ]]; then
    printf "${CHECK} Error reported to calling script\n"
  else
    printf "${CROSS} Error not reported to calling script\n"
    ALL_PASS=false
  fi

  # output_logs will be accessible here.  The report makes the most intuitive sense
  # when presented here.

  # Verify output logs have the expected output message.

  if [[ "${output_logs:-}" =~ "raise-build-error:project failed" ]]; then
    printf "${CHECK} Expected build-failure report was emitted\n"
  else
    printf "${CROSS} Expected build-failure report was not emitted\n"
    ALL_PASS=false
  fi

  if [[ $ALL_PASS == true ]]; then
    builder_report success error
    exit 0
  else
    builder_report failure error
    exit 1
  fi
}

# Note:  if this test is run, no other ones after it may execute!
if builder_has_action error; then
  # clear base traps
  trap - err
  trap - exit

  echo "$HR"

  # The `|| true` disables any trap shenanigans.
  # Needed to capture the log messages.
  output_logs=`./trap-test-builds.sh raise-build-error` || true

  trap error-test-matcher err

  # Relies on the `trap` - no further commands outside the handler will be processed.
  # Also emits the log messages (but we can't capture them here)
  ./trap-test-builds.sh raise-build-error

  # In case the expected error isn't emitted.  Will emit the report-failure.
  error-test-matcher
fi

########

warning-test-matcher() {
  ERR_CODE=$?
  ALL_PASS=true

  echo "$HR"

  if [[ $ERR_CODE == 0 ]]; then
    printf "${CHECK} Error not reported to calling script\n"
  else
    printf "${CROSS} Error reported to calling script\n"
    ALL_PASS=false
  fi

  # output_logs will be accessible here.  The report makes the most intuitive sense
  # when presented here.

  # Verify output logs have the expected output message.

  if [[ "${output_logs:-}" =~ "unreported-action:project never reported" ]]; then
    printf "${CHECK} Expected warning report was emitted\n"
  else
    printf "${CROSS} Expected warning report was not emitted\n"
    ALL_PASS=false
  fi

  if [[ $ALL_PASS == true ]]; then
    builder_report success incomplete
    exit 0
  else
    builder_report failure incomplete
    exit 1
  fi
}

if builder_has_action incomplete; then
  # clear base traps
  trap - err
  trap - exit

  echo "$HR"

  # The `|| true` disables any trap shenanigans.
  output_logs=`./trap-test-builds.sh unreported-action` || true

  trap warning-test-matcher err

  # Relies on the `trap` - no further commands outside the handler will be processed.
  ./trap-test-builds.sh unreported-action

  warning-test-matcher
fi