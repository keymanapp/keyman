#!/usr/bin/env bash
#
# This script tests build-utils-ci.sh
#

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
# END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# Tests

builder_echo "${COLOR_BLUE}## Testing: builder_pull_get_details with \$TEAMCITY_PR_NUMBER not set${COLOR_RESET}"
if ! builder_pull_get_details; then
  builder_echo "PASS: builder_pull_get_details should get no details if \$TEAMCITY_PR_NUMBER is not set"
else
  builder_die "FAIL: builder_pull_get_details should get no details if \$TEAMCITY_PR_NUMBER is not set"
fi


builder_echo "${COLOR_BLUE}## Testing: builder_pull_get_details with \$TEAMCITY_PR_NUMBER=master${COLOR_RESET}"
TEAMCITY_PR_NUMBER=master
if ! builder_pull_get_details; then
  builder_echo "PASS: builder_pull_get_details should get no details if \$TEAMCITY_PR_NUMBER=master"
else
  builder_die "FAIL: builder_pull_get_details should get no details if \$TEAMCITY_PR_NUMBER=master"
fi


builder_echo "${COLOR_BLUE}## Testing: builder_pull_get_details with \$TEAMCITY_PR_NUMBER=$TEAMCITY_PR_NUMBER${COLOR_RESET}"
TEAMCITY_PR_NUMBER=7248
if ! builder_pull_get_details; then
  builder_die "FAIL: builder_pull_get_details was unable to read and parse PR details for #$TEAMCITY_PR_NUMBER from GitHub"
fi

# print metadata from PR
builder_echo "  pull number: $builder_pull_number"
builder_echo "  title:       $builder_pull_title"
builder_echo "  labels:      ${builder_pull_labels[*]}"

builder_echo "PASS: builder_pull_get_details with \$TEAMCITY_PR_NUMBER=$TEAMCITY_PR_NUMBER"


builder_echo "${COLOR_BLUE}## Testing: builder_pull_has_label epic-ldml${COLOR_RESET}"
if ! builder_pull_has_label epic-ldml; then
  builder_die "FAIL: builder_pull_has_label epic-ldml"
fi

builder_echo "PASS: builder_pull_has_label epic-ldml"

builder_echo "All tests passed."
