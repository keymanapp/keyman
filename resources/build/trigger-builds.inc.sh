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
  curl -s  --write-out '\n' --header "Authorization: Bearer $TEAMCITY_TOKEN" \
    -X POST \
    -H "Content-Type: application/xml" \
    -H "Accept: application/json" \
    -H "Origin: $TEAMCITY_SERVER" \
    $TEAMCITY_SERVER/app/rest/buildQueue \
    -d "$command"
}

function triggerGitHubActionsBuild() {
  local IS_TEST_BUILD="$1"
  local GITHUB_ACTION="$2"
  local GIT_BRANCH="${3:-master}"
  local GIT_REF GIT_SHA

  local GITHUB_SERVER=https://api.github.com/repos/keymanapp/keyman/dispatches

  if [ "${action:-""}" == "commit" ]; then
    # This will only be true if we created and pushed a tag
    GIT_REF="refs/tags/release@$VERSION_WITH_TAG"
    GIT_SHA="$(git rev-parse "${GIT_REF}")"
    GIT_EVENT_TYPE="${GITHUB_ACTION}: release@${VERSION_WITH_TAG}"
  elif [[ $GIT_BRANCH != stable-* ]] && [[ $GIT_BRANCH =~ [0-9]+ ]]; then
    GIT_REF="refs/pull/${GIT_BRANCH}/merge"
    GIT_SHA="$(git rev-parse "refs/pull/${GIT_BRANCH}/head")"
    GIT_EVENT_TYPE="${GITHUB_ACTION}: PR #${GIT_BRANCH}"
    GIT_BRANCH="PR-${GIT_BRANCH}"
  else
    GIT_REF="refs/heads/${GIT_BRANCH}"
    GIT_SHA="$(git rev-parse "${GIT_REF}")"
    GIT_EVENT_TYPE="${GITHUB_ACTION}: ${GIT_BRANCH}"
  fi

  local DATA="{\"event_type\": \"$GIT_EVENT_TYPE\", \
      \"client_payload\": { \
        \"ref\": \"$GIT_REF\", \
        \"sha\": \"$GIT_SHA\", \
        \"branch\": \"$GIT_BRANCH\", \
        \"isTestBuild\": \"$IS_TEST_BUILD\" \
    }}"

  echo "GitHub Action Data: $DATA"

  # adjust indentation for output of curl
  echo -n "     "
  curl --silent --write-out '\n' \
    --request POST \
    --header "Accept: application/vnd.github+json" \
    --header "Authorization: token $GITHUB_TOKEN" \
    --data "$DATA" \
    $GITHUB_SERVER
}
