#!/bin/bash

#
# Tell TeamCity to trigger new builds
#

function triggerBuilds() {
  local base=`git branch --show-current`
  eval TEAMCITY_VCS_ID='${'vcs_$base'}'
  echo "base=$base, TEAMCITY_VCS_ID=${TEAMCITY_VCS_ID}"

  for platform in ${available_platforms[@]}; do
    eval builds='(${'bc_${base}_${platform}'[@]})'
    for build in "${builds[@]}"; do
      if [[ $build == "" ]]; then continue; fi
      if [ "${build:(-8)}" == "_Jenkins" ]; then
        local job=${build%_Jenkins}
        echo Triggering Jenkins build "$job" "$base" "true"
        triggerJenkinsBuild "$job" "$base" "true"
      else
        echo Triggering TeamCity build $build $TEAMCITY_VCS_ID
        triggerTeamCityBuild $build $TEAMCITY_VCS_ID
      fi
    done
  done
}

function triggerTeamCityBuild() {
  local TEAMCITY_BUILDTYPE="$1"
  local TEAMCITY_VCS_ID="$2"

  if [[ $# -gt 2 ]]; then
    local TEAMCITY_BRANCH_NAME="$3"
    #debug echo "  Triggering build for: $TEAMCITY_BRANCH_NAME"
    TEAMCITY_BRANCH_NAME="branchName='$TEAMCITY_BRANCH_NAME' defaultBranch='false'"
  else
    local TEAMCITY_BRANCH_NAME=
  fi

  local GIT_OID=`git rev-parse HEAD`
  local TEAMCITY_SERVER=https://build.palaso.org

  local command="<build $TEAMCITY_BRANCH_NAME><buildType id='$TEAMCITY_BUILDTYPE' /><lastChanges><change vcsRootInstance='$TEAMCITY_VCS_ID' locator='version:$GIT_OID,buildType:(id:$TEAMCITY_BUILDTYPE)'/></lastChanges></build>"
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

  local FORCE=""
  if [ "${3:-false}" == "true" ]; then
    FORCE=", \"force\": true"
  fi

  if [[ $JENKINS_BRANCH =~ [0-9]+ ]]; then
    JENKINS_BRANCH="PR-${JENKINS_BRANCH}"
  fi

  local OUTPUT=$(curl --silent --write-out '\n' \
    -X POST \
    --header "token: $JENKINS_TOKEN" \
    --header "Content-Type: application/json" \
    $JENKINS_SERVER/generic-webhook-trigger/invoke \
    --data "{ \"project\": \"$JENKINS_JOB/$JENKINS_BRANCH\", \"branch\": \"$JENKINS_BRANCH\" $FORCE }")

  if echo "$OUTPUT" | grep -q "\"triggered\":true"; then
    echo -n "     job triggered: "
  else
    echo -n "     triggering failed: "
  fi

  # Strip {"jobs":{ from the beginning of OUTPUT
  OUTPUT=${OUTPUT#\{\"jobs\":\{}
  # Split json string to lines with one job each
  local jobs
  IFS='|' jobs=(${OUTPUT//\},\"pipeline/\},|\"pipeline})
  # Find job that actually got triggered (or that we should have triggered)
  for line in "${jobs[@]}"; do
    if [[ $line == \"$JENKINS_JOB/$JENKINS_BRANCH* ]]; then
      echo "$line"
    fi
  done
}
