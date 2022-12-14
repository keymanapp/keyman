#!/usr/bin/env bash

#
# Tell TeamCity to trigger new builds
#

function triggerBuilds() {
  local base
  base=$(git branch --show-current)
  # convert stable-14.0 to stable_14_0 to fit in with the definitions
  # in trigger-definitions.inc.sh
  local bcbase=${base//[-.]/_}
  eval TEAMCITY_VCS_ID='${'vcs_$bcbase'}'
  echo "base=$base, TEAMCITY_VCS_ID=${TEAMCITY_VCS_ID}"


  for platform in "${available_platforms[@]}"; do
    eval builds='(${'"bc_${bcbase}_${platform}"'[@]})'
    for build in "${builds[@]}"; do
      if [[ $build == "" ]]; then continue; fi
      if [ "${build:(-8)}" == "_Jenkins" ]; then
        local job=${build%_Jenkins}
        echo Triggering Jenkins build "$job" "$base" "true"
        triggerJenkinsBuild "$job" "$base" "true"
      else
        echo Triggering TeamCity build false "$build" "$TEAMCITY_VCS_ID" "$base"
        triggerTeamCityBuild false "$build" "$TEAMCITY_VCS_ID" "$base"
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

  local TEAMCITY_SERVER=https://build.palaso.org
  local GIT_OID
  GIT_OID=$(git rev-parse HEAD)

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

function triggerJenkinsBuild() {
  local JENKINS_JOB="$1"
  local JENKINS_BRANCH="${2:-master}"

  local JENKINS_SERVER=https://jenkins.lsdev.sil.org
  local GIT_TAG="release-$VERSION_WITH_TAG"

  local FORCE=""
  if [ "${3:-false}" == "true" ]; then
    FORCE=", \"force\": true"
  fi

  local TAG=""
  # This will only be true if we created and pushed a tag
  if [ "${action:-""}" == "commit" ]; then
    TAG=", \"tag\": \"$GIT_TAG\", \"tag2\": \"$GIT_TAG\""
  fi

  if [[ $JENKINS_BRANCH != stable-* ]] && [[ $JENKINS_BRANCH =~ [0-9]+ ]]; then
    JENKINS_BRANCH="PR-${JENKINS_BRANCH}"
  fi

  local OUTPUT
  OUTPUT=$(curl --silent --write-out '\n' \
    -X POST \
    --header "token: $JENKINS_TOKEN" \
    --header "Content-Type: application/json" \
    $JENKINS_SERVER/generic-webhook-trigger/invoke \
    --data "{ \"project\": \"$JENKINS_JOB/$JENKINS_BRANCH\", \"branch\": \"$JENKINS_BRANCH\" $TAG $FORCE }")

  if echo "$OUTPUT" | grep -q "\"triggered\":true"; then
    echo -n "     job triggered: "
  else
    echo -n "     triggering failed: "
  fi

  # Strip {"jobs":{ from the beginning of OUTPUT
  OUTPUT=${OUTPUT#\{\"jobs\":\{}
  # Split json string to lines with one job each
  local jobs count
  count=0
  IFS='|' jobs=(${OUTPUT//\},\"pipeline/\},|\"pipeline})
  # Find job that actually got triggered (or that we should have triggered)
  for line in "${jobs[@]}"; do
    if [[ $line == \"$JENKINS_JOB/$JENKINS_BRANCH* ]]; then
      echo "$line"
      count=$((++count))
    fi
  done
  if (( count < 1 )); then
    # DEBUG
    echo -n "$OUTPUT"

    echo
  fi
}
