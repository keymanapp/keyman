#!/usr/bin/env bash

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
# END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

##############

CHECK="${COLOR_GREEN}✔${COLOR_RESET}" # ✔
CROSS="${COLOR_RED}❌${COLOR_RESET}" # ❌

builder_describe \
  "Provides actions useful for build-utils.sh trap-functionality unit testing.\nOnly one action may be set at a time." \
  "error" \
  "error-in-function" \
  "incomplete"

builder_parse "$@"

###

HR="------------------------------------------------"

active_test=
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

  if [[ "${output_logs:-}" =~ "raise-build-error failed" ]]; then
    printf "${CHECK} Expected build-failure report was emitted\n"
  else
    printf "${CROSS} Expected build-failure report was not emitted\n"
    ALL_PASS=false
  fi

  # Let's not re-trap ourselves from the trap!
  trap - err exit

  if [[ $ALL_PASS == true ]]; then
    builder_finish_action success $active_test
    exit 0
  else
    builder_finish_action failure $active_test
    exit 1
  fi
}

# Note:  if this test is run, no other ones after it may execute!
if builder_start_action error; then
  # clear base traps
  trap - err
  trap - exit

  active_test=error

  echo "$HR"

  # The `|| true` disables any trap shenanigans.
  # Needed to capture the log messages.
  output_logs=`$THIS_SCRIPT_PATH/trap-test-builds.test.sh raise-build-error` || true

  trap error-test-matcher err exit

  # Relies on the `trap` - no further commands outside the handler will be processed.
  # Also emits the log messages (but we can't capture them here)
  $THIS_SCRIPT_PATH/trap-test-builds.test.sh raise-build-error

  # In case the expected error isn't emitted.  Will emit the report-failure.
  error-test-matcher
fi

function-with-error() {
  $THIS_SCRIPT_PATH/trap-test-builds.test.sh raise-build-error
}

# Note:  if this test is run, no other ones after it may execute!
if builder_start_action error-in-function; then
  # clear base traps
  trap - err
  trap - exit

  active_test=error-in-function

  echo "$HR"

  # The `|| true` disables any trap shenanigans.
  # Needed to capture the log messages.
  output_logs=`function-with-error` || true

  trap error-test-matcher err exit

  # Relies on the `trap` - no further commands outside the handler will be processed.
  # Also emits the log messages (but we can't capture them here)
  function-with-error

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

  if [[ "${output_logs:-}" =~ "unreported-action:* never reported" ]]; then
    printf "${CHECK} Expected warning report was emitted\n"
  else
    printf "${CROSS} Expected warning report was not emitted\n"
    ALL_PASS=false
  fi

  # Let's not re-trap ourselves from the trap!
  trap - err exit

  if [[ $ALL_PASS == true ]]; then
    builder_finish_action success incomplete
    exit 0
  else
    builder_finish_action failure incomplete
    exit 1
  fi
}

if builder_start_action incomplete; then
  # clear base traps
  trap - err
  trap - exit

  echo "$HR"

  # The `|| true` disables any trap shenanigans.
  output_logs=`$THIS_SCRIPT_PATH/trap-test-builds.test.sh unreported-action` || true

  trap warning-test-matcher err

  # Relies on the `trap` - no further commands outside the handler will be processed.
  $THIS_SCRIPT_PATH/trap-test-builds.test.sh unreported-action

  warning-test-matcher
fi