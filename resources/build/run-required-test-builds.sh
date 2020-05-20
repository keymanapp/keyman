#!/bin/bash
#
# Determine if we need to do a build based on rules in
# trigger-definitions.inc.sh, rather than calculating changes in
# TeamCity. If a build is needed, then we ask TeamCity to
# start the build.
#

set -e
set -u

if [[ $# -lt 1 ]]; then
  echo 'Usage: run-required-test-build.sh pull-request-number|branch'
  echo '  where branch can be master, beta'
  exit 1
else
  PRNUM="$1"
fi

function debug_echo() {
  #echo "DEBUG: $1"
  true
}

#
# This must be run under a TeamCity environment in order to receive all variables
#

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$(dirname "$THIS_SCRIPT")/trigger-definitions.inc.sh"
. "$(dirname "$THIS_SCRIPT")/trigger-builds.inc.sh"
. "$(dirname "$THIS_SCRIPT")/jq.inc.sh"

#
# Iterate through the platforms 'array' passed in and
# run builds associated with each platform found
#

function triggerTestBuilds() {
  # Note: we always run builds for 'all' platforms
  local platforms=( `echo "all $1"` )
  local branch="$2"
  local force="${3:-false}"

  for platform in "${platforms[@]}"; do
    echo "# $platform: checking for changes"
    eval test_builds='(${'bc_test_$platform'[@]})'
    for test_build in "${test_builds[@]}"; do
      if [[ $test_build == "" ]]; then continue; fi
      if [ "${test_build:(-8)}" == "_Jenkins" ]; then
        local job=${test_build%_Jenkins}
        echo "  -- Triggering build configuration $job/$branch on Jenkins"
        triggerJenkinsBuild "$job" "$branch" "$force"
      else
        echo "  -- Triggering build configuration $test_build on teamcity"
        triggerTeamCityBuild "$test_build" "$vcs_test" "$branch"
      fi
    done
  done
}

#
# Following is not an error; this script can run against master/beta/stable
# branches as well as pull requests
#

if [[ ! "$PRNUM" =~ ^[[:digit:]]+$ ]]; then
  # branch name is 'master', 'beta' [, or 'stable' -- in the future]
  echo ". Branch $PRNUM needs to pass tests on all platforms."
  triggerTestBuilds "`echo ${available_platforms[@]}`" "$PRNUM" "true"
  exit 0
fi

#
# Find the base and head of the pull request from GitHub (while we can
# just use the current branch name, this lets us test the script against
# different pull requests; and there is no way to know which base is
# targeted by the PR other than to ask GitHub, anyway.)
#

echo ". Get information about pull request #$PRNUM from GitHub"
prinfo=`curl -s -H "User-Agent: @keymanapp" https://api.github.com/repos/keymanapp/keyman/pulls/$PRNUM`
prbase=`echo ${prinfo} | "$JQ" -r '.base.ref'`
prhead=`echo ${prinfo} | "$JQ" -r '.head.ref'`

debug_echo "PRNUM: $PRNUM"
debug_echo "BASE: $prbase"
debug_echo "HEAD: $prhead"

#
# By default, TeamCity does not fetch any remote branch information
# so let's grab the remote branch info before we try and run a diff
#

# (Ensure we are within the repository before running git calls)
echo ". Ensure our local branch is up to date"
pushd "$KEYMAN_ROOT" > /dev/null
git fetch origin > /dev/null

#
# Then get a list of changed files between BASE of the branch and the branch itself
# We work from origin so we don't need the branches in our local copy
#

echo ". Get list of changed files in the pull request"
prfiles=`git diff --name-only "origin/$prbase"..."origin/$prhead" || ( if [ $? == 128 ]; then echo abort; else exit $?; fi )`
if [ "$prfiles" == "abort" ]; then
  # Don't trigger any builds, exit with success
  echo "Remote branch origin/$prhead has gone away; probably an automatic pull request. Skipping build."
  exit 0
fi

debug_echo "Files found: ${prfiles[*]}"

popd > /dev/null

#
# Find the platforms that have changes based on the watch_ variables in trigger-definitions.inc.sh
#

echo ". Find platforms that have changes"
build_platforms=()

# Scan the files found
while IFS= read -r line; do
  # for each platform
  for platform in "${available_platforms[@]}"; do
    if [[ ! " ${build_platforms[@]} " =~ " $platform " ]]; then
      # Which platform are we watching?
      eval watch='$'watch_$platform
      # Add common patterns to the watch list
      watch="^($platform|(oem/[^/]+/$platform)|resources|$watch)"
      if [[ "$line" =~ $watch ]]; then
        build_platforms+=($platform)
      fi
    fi
  done
done <<< "$prfiles"

debug_echo "Build platforms: ${build_platforms[*]}"
#
# Start the test builds
#

echo ". Start test builds"
triggerTestBuilds "`echo ${build_platforms[@]}`" "$PRNUM"

exit 0
