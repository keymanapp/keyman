#!/bin/bash

#
# Tell TeamCity to trigger new builds
#

function triggerBuilds() {
  local base=`git branch --show-current`
  if [[ $base == master ]]; then
    local TEAMCITY_BUILDTYPES=( KeymanAndroid_Build Keyman_iOS_Master KeymanLinux_Master KeymanMac_Master Keyman_Build Keymanweb_Build )
    local TEAMCITY_VCS_ID=HttpsGithubComKeymanappKeyman
  elif [[ $base == beta ]]; then
    local TEAMCITY_BUILDTYPES=( KeymanAndroid_Beta Keyman_iOS_Beta KeymanLinux_Beta KeymanMac_Beta KeymanDesktop_Beta Keymanweb_Beta )
    local TEAMCITY_VCS_ID=Keyman_KeymanappKeymanBeta
  else
    exit 1
  fi

  for TEAMCITY_BUILDTYPE in "${TEAMCITY_BUILDTYPES[@]}"; do
    triggerBuild $TEAMCITY_BUILDTYPE $TEAMCITY_VCS_ID
  done
}

function triggerBuild() {
  local TEAMCITY_BUILDTYPE="$1"
  local TEAMCITY_VCS_ID="$2"

  local GIT_OID=`git rev-parse HEAD`
  local TEAMCITY_SERVER=https://build.palaso.org

  curl -s --header "Authorization: Bearer $TEAMCITY_TOKEN" \
    -X POST \
    -H "Content-Type: application/xml" \
    -H "Accept: application/json" \
    -H "Origin: $TEAMCITY_SERVER" \
    $TEAMCITY_SERVER/app/rest/buildQueue \
    -d "<build><buildType id='$TEAMCITY_BUILDTYPE' /><lastChanges><change vcsRootInstance='$TEAMCITY_VCS_ID' locator='version:$GIT_OID,buildType:(id:$TEAMCITY_BUILDTYPE)'/></lastChanges></build>"
}
