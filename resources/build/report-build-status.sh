#!/bin/bash

set -e
set -u

#
# This must be run under a TeamCity environment in order to receive all variables
#

if [[ $# -lt 6 ]]; then
  echo Usage: report-build-status.sh mode GitHub-token git-build-sha server-url build-id tc-user:password
fi

MODE="$1"
TOKEN="$2"     # GitHub token
BUILD_SHA="$3" # %build.vcs.number%
TCSERVER="$4"  # %teamcity.serverUrl%
TCBUILDID="$5" # %teamcity.build.id%
TCUSER="$6"    # %teamcity_user%:%teamcity_password%

if [[ "$MODE" == "start" ]]; then
  STATE="pending"
  DESCRIPTION="Build started"
else
  # MODE = finish
  BUILDSTATE=`curl -s --request GET "$TCSERVER/httpAuth/app/rest/builds/id:$TCBUILDID/status" --basic --user "$TCUSER"`
  DESCRIPTION=`curl -s --request GET "$TCSERVER/httpAuth/app/rest/builds/id:$TCBUILDID/statusText" --basic --user "$TCUSER"`

  if [[ "$BUILDSTATE" == "SUCCESS" ]]; then
    STATE="success"
  elif [[ "$BUILDSTATE" == "UNKNOWN" ]]; then
    STATE="success"
  elif [[ "$BUILDSTATE" == "FAILURE" ]]; then
    STATE="failure"
  else
    STATE="success"
    DESCRIPTION="Unknown build state '$BUILDSTATE'; $DESCRIPTION"
  fi
fi

LOGURL="$TCSERVER/viewLog.html?buildId=$TCBUILDID"
CONTEXT="$TEAMCITY_BUILDCONF_NAME ($TEAMCITY_PROJECT_NAME)"

curl -s -H "Authorization: token $TOKEN" -H "User-Agent: @keymanapp" https://api.github.com/repos/keymanapp/keyman/statuses/$BUILD_SHA --request "POST" --data "
  {
    \"state\": \"$STATE\",
    \"target_url\": \"$LOGURL\",
    \"description\": \"$DESCRIPTION\",
    \"context\": \"$CONTEXT\"
  }" 