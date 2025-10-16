# shellcheck shell=bash
# Keyman is copyright (C) SIL Global. MIT License.

#
# Runs eslint, builds tests, and then runs tests with mocha + c8 (coverage)
#
# Usage:
#   builder_run_action  test    typescript_run_eslint_mocha_tests [coverage_threshold]
# Parameters:
#   1: coverage_threshold   optional, minimum coverage for c8 to pass tests,
#                           defaults to 90 (percent)
typescript_run_eslint_mocha_tests() {
  local MOCHA_FLAGS=()
  local TEST_DIR

  if builder_is_running_on_teamcity; then
    # we're running in TeamCity
    MOCHA_FLAGS+=(--reporter "${KEYMAN_ROOT}/common/test/resources/mocha-teamcity-reporter/teamcity.cjs" --reporter-options parentFlowId="unit_tests")
    echo "##teamcity[flowStarted flowId='unit_tests']"
  fi

  eslint .

  # TODO: Unify test directory names (#14878)
  if [[ -d test/ ]]; then
    TEST_DIR="test/"
  elif [[ -d tests/ ]]; then
    TEST_DIR="tests/"
  else
    builder_die "No test/ or tests/ directory found."
  fi

  tsc --build "${TEST_DIR}"

  local THRESHOLD_PARAMS=
  local C8_THRESHOLD=
  if [[ $# -gt 0 ]]; then
    C8_THRESHOLD=$1
    THRESHOLD_PARAMS="--lines ${C8_THRESHOLD} --statements ${C8_THRESHOLD} --branches ${C8_THRESHOLD} --functions ${C8_THRESHOLD}"
  else
    # Seems like a bug in TeamCity reporter if we don't list default thresholds,
    # see #13418.
    #
    # Making branch and function thresholds slightly lower, because the default
    # for c8 is 0 for these anyway.
    THRESHOLD_PARAMS="--lines 90 --statements 90 --branches 80 --functions 80"
  fi

  c8 --reporter=lcov --reporter=text --exclude-after-remap --check-coverage ${THRESHOLD_PARAMS} mocha "${MOCHA_FLAGS[@]}" "${builder_extra_params[@]}"

  if [[ ! -z "${C8_THRESHOLD}" ]]; then
    builder_echo warning "Coverage thresholds are currently ${C8_THRESHOLD}%, which is lower than ideal."
    builder_echo warning "Please increase threshold in build.sh as test coverage improves."
  fi

  if builder_is_running_on_teamcity; then
    # we're running in TeamCity
    echo "##teamcity[flowFinished flowId='unit_tests']"
  fi
}
