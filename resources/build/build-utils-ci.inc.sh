#!/usr/bin/env bash
#
# This script gets CI / pull request details for builds, part of the build-utils
# builder_ suite of functions. All functions and variables in this file have the
# prefix builder_pull_.
#

. "$REPO_ROOT/resources/build/jq.inc.sh"

#
# Returns 0 if current build is in CI and triggered from a pull request. If it
# returns 0, then a call is made to GitHub to get pull request details, and the
# PR details are added to $builder_pull_title, $builder_pull_number, and
# the $builder_pull_labels array.
#
# Note that the GitHub REST call is made with credentials if $GITHUB_TOKEN is
# available; without credentials it will be subject to IP-based rate limits.
#
builder_pull_get_details() {
  builder_pull_title=
  builder_pull_number=
  builder_pull_labels=()
  if [[ ! ${TEAMCITY_PR_NUMBER-} =~ ^[0-9]+$ ]]; then
    return 1
  fi

  if [ -z "${GITHUB_TOKEN-}" ]; then
    local pull_data=`curl -s https://api.github.com/repos/keymanapp/keyman/pulls/$TEAMCITY_PR_NUMBER`
  else
    local pull_data=`curl -H "Authorization: Bearer $GITHUB_TOKEN" -s https://api.github.com/repos/keymanapp/keyman/pulls/$TEAMCITY_PR_NUMBER`
  fi

  builder_pull_title=`echo $pull_data | $JQ .title`

  # Simple poor bash test if data returned from API is valid
  if [[ -z builder_pull_title ]]; then
    return 1
  fi

  builder_pull_number=$TEAMCITY_PR_NUMBER
  builder_pull_labels=(`echo $pull_data | $JQ -jr '.labels[].name|.," "'`)

  return 0
}


#
# Returns 0 if the current PR has a particular label. Requires
# builder_pull_get_details to have successfully run first
#
# Usage:
#   builder_pull_has_label "label-name"
# Parameters
#   1: $label       label to test
#
function builder_pull_has_label() {
  local label="$1"
  if [[ " ${builder_pull_labels[*]} " =~ " $label " ]]; then
    return 0
  fi
  return 1
}
