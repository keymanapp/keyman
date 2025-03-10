#!/usr/bin/env bash

#
# Tell TeamCity to trigger new builds
#

function triggerBuilds() {
  local base=`git branch --show-current`
  # convert stable-14.0 to stable_14_0 to fit in with the definitions
  # in trigger-definitions.inc.sh
  local bcbase=${base//[-.]/_}
  eval TEAMCITY_VCS_ID='${'vcs_$bcbase'}'
  echo "base=$base, TEAMCITY_VCS_ID=${TEAMCITY_VCS_ID}"


  for platform in ${available_platforms[@]}; do
    eval builds='(${'bc_${bcbase}_${platform}'[@]})'
    for build in "${builds[@]}"; do
      if [[ $build == "" ]]; then continue; fi
      if [ "${build:(-7)}" == "_GitHub" ]; then
        local job=${build%_GitHub}
        echo Triggering GitHub action build "$job" "$base"
        triggerGitHubActionsBuild false "$job" "$base"
      else
        echo Triggering TeamCity build false $build $TEAMCITY_VCS_ID $base
        triggerTeamCityBuild false $build $TEAMCITY_VCS_ID $base
      fi
    done
  done
}

function triggerTeamCityBuild() {
  local isTestBuild="$1"
  local TEAMCITY_BUILDTYPE="$2"
  local TEAMCITY_VCS_ID="$3"

  if [[ $# -gt 3 ]]; then
    local TEAMCITY_BRANCH_NAME="$4"
    #debug echo "  Triggering build for: $TEAMCITY_BRANCH_NAME"
    TEAMCITY_BRANCH_NAME="branchName='$TEAMCITY_BRANCH_NAME' defaultBranch='false'"
  else
    local TEAMCITY_BRANCH_NAME=
  fi

  local GIT_OID=`git rev-parse HEAD`
  local TEAMCITY_SERVER=https://build.palaso.org

  local command

  if $isTestBuild; then
    command="<build $TEAMCITY_BRANCH_NAME><buildType id='$TEAMCITY_BUILDTYPE' /></build>"
  else
    command="<build $TEAMCITY_BRANCH_NAME><buildType id='$TEAMCITY_BUILDTYPE' /><lastChanges><change vcsRootInstance='$TEAMCITY_VCS_ID' locator='version:$GIT_OID'/></lastChanges></build>"
  fi
  echo "TeamCity Build Command: $command"

  # adjust indentation for output of curl
  echo -n "     "
  call_curl "$TEAMCITY_SERVER/app/rest/buildQueue" \
    --header "Authorization: Bearer $TEAMCITY_TOKEN" \
    -X POST \
    -H "Content-Type: application/xml" \
    -H "Accept: application/json" \
    -H "Origin: $TEAMCITY_SERVER" \
    -d "$command"
}

function triggerGitHubActionsBuild() {
  local IS_TEST_BUILD="$1"
  local GITHUB_ACTION="$2"
  local GIT_BRANCH="${3:-master}"
  local GIT_BASE_BRANCH="${GIT_BRANCH}"
  local GIT_USER="keyman-server"
  local GIT_BUILD_SHA GIT_BASE_REF JSON

  local GITHUB_SERVER=https://api.github.com/repos/keymanapp/keyman

  if [ "${action:-""}" == "commit" ]; then
    # This will only be true if we created and pushed a tag
    GIT_BUILD_SHA="$(git rev-parse "refs/tags/release@$VERSION_WITH_TAG")"
    GIT_BASE_REF="$(git rev-parse "${GIT_BUILD_SHA}^")"
    GIT_EVENT_TYPE="${GITHUB_ACTION}: release@${VERSION_WITH_TAG}"
  elif [[ $GIT_BRANCH != stable-* ]] && [[ $GIT_BRANCH =~ [0-9]+ ]]; then
    local JSON=$(call_curl "${GITHUB_SERVER}/pulls/${GIT_BRANCH}" --header "Authorization: token $GITHUB_TOKEN")

    GIT_BUILD_SHA="$(echo "$JSON" | $JQ -r '.head.sha')"
    GIT_EVENT_TYPE="${GITHUB_ACTION}: PR #${GIT_BRANCH}"
    GIT_USER="$(echo "$JSON" | $JQ -r '.user.login')"
    GIT_BASE_BRANCH="$(echo "$JSON" | $JQ -r '.base.ref')"
    GIT_BASE_REF="$(echo "$JSON" | $JQ -r '.base.sha')"
    GIT_BRANCH="PR-${GIT_BRANCH}"
  else
    GIT_BUILD_SHA="$(git rev-parse "refs/heads/${GIT_BRANCH}")"
    GIT_BASE_REF="$(git rev-parse "${GIT_BUILD_SHA}^")"
    GIT_EVENT_TYPE="${GITHUB_ACTION}: ${GIT_BRANCH}"
  fi

  local DATA="{
    \"event_type\": \"$GIT_EVENT_TYPE\", \
    \"client_payload\": { \
      \"buildSha\": \"$GIT_BUILD_SHA\", \
      \"branch\": \"$GIT_BRANCH\", \
      \"baseBranch\": \"$GIT_BASE_BRANCH\", \
      \"baseRef\": \"$GIT_BASE_REF\", \
      \"user\": \"$GIT_USER\", \
      \"isTestBuild\": \"$IS_TEST_BUILD\" \
    }}"

  echo "GitHub Action Data: $DATA"

  if [[ "$GIT_BUILD_SHA" == null ]]; then
    echo "Invalid GIT_BUILD_SHA"
    exit 1
  fi

  # adjust indentation for output of curl
  echo -n "     "
  call_curl "${GITHUB_SERVER}/dispatches" \
    --request POST \
    --header "Accept: application/vnd.github+json" \
    --header "Authorization: token $GITHUB_TOKEN" \
    --data "$DATA"

}

#
# Error handling and reporting for curl + default parameters
# We cannot use --fail-with-body yet due to older curl versions
# on infrastructure
#
function call_curl() {
  local url="$1"
  shift

  local curl_result
  local http_code
  local output_file=$(mktemp)

  set +e
  if [[ $# -gt 0 ]]; then
    http_code=$(curl --silent --output $output_file --no-progress-meter --write-out "%{http_code}" "$@" "$url")
  else
    http_code=$(curl --silent --output $output_file --no-progress-meter --write-out "%{http_code}" "$url")
  fi
  curl_result=$?
  if [[ ${curl_result} != 0 ]]; then
    echo "curl call to '${url}' failed with code '${curl_result}'" >& 2
    >&2 cat $output_file
    rm -f $output_file
    exit $curl_result
  fi

  if [[ ${http_code} -lt 200 || ${http_code} -gt 299 ]] ; then
    echo "curl call to '${url}' failed with HTTP error code '${http_code}'" >& 2
    >&2 cat $output_file
    rm -f $output_file
    exit 22
  fi

  set -e

  cat $output_file
  rm -f $output_file
}
