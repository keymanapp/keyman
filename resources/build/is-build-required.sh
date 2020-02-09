#!/bin/bash
#
# Determine if we need to do a build based on rules in
# build-triggers.sh, rather than calculating changes in
# TeamCity. If a build is not needed, then cancel the 
# build in TeamCity
#

set -e
set -u

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$(dirname "$THIS_SCRIPT")/build-triggers.sh"

#
# Parameter test
#

invalid=false
platform="$TEAMCITY_PLATFORM"
PRNUM="$TEAMCITY_PR_NUMBER"

if [[ ! "$platform" =~ android|ios|linux|mac|web|windows ]]; then
  echo "Invalid platform $platform"
  invalid=true
fi

if [ "$platform" == "android" ]; then
  echo "Testing cancellation of build! Skipping build."
  # echo "##teamcity[buildStatus status='SUCCESS' text='Testing cancellation of build - status message! Skipping build.']"
  echo "##teamcity[buildStop comment='Testing cancellation of build - cancel message! Skipping build.']"
  ./report-build-status.sh finish "$@"
  exit 1
fi

if [[ $invalid == true ]]; then
  echo "Usage: $(basename "$THIS_SCRIPT") [platform [prnum]]"
  echo "platform can be one of:"
  echo "  android"
  echo "  ios"
  echo "  linux"
  echo "  mac"
  echo "  web"
  echo "  windows (includes Desktop + Developer)"
  echo "If not passed, then platform is read from env TEAMCITY_PLATFORM,"
  echo "and PRNUM is read from env TEAMCITY_PR_NUMBER"
  exit 1
fi

#
# Following is not an error; this script can run against master/beta/stable
# branches as well as pull requests
#

if [[ ! "$PRNUM" =~ ^[[:digit:]]+$ ]]; then
  echo "Branch spec $PRNUM is not a pull request number; not stopping build."
  exit 0
fi

#
# Make sure our local index is up to date with origin
#

pushd "$KEYMAN_ROOT" > /dev/null

git fetch origin

#
# Find the base and head of the pull request from GitHub
#

prinfo=`curl -s -H "User-Agent: @keymanapp" https://api.github.com/repos/keymanapp/keyman/pulls/$PRNUM`
prbase=`echo ${prinfo} | sed -E 's/.+"base".+"ref":"([^"]+)".+/\1/'`
prhead=`echo ${prinfo} | sed -E 's/.+"head".+"ref":"([^"]+)".+"base".+"ref":.+/\1/'`

# echo "Base of $PRNUM ($prhead) is $prbase"

#
# Then get a list of changed files between BASE of the branch and the branch itself
# We work from origin so we don't need the branches checked out ourselves
#

prfiles=`git diff "origin/$prbase"..."origin/$prhead" --name-only || ( if [ $? == 128 ]; then echo abort; else exit $?; fi )`
if [ "$prfiles" == "abort" ]; then
  # Cancel the build
  echo "Remote branch origin/$prhead has gone away; probably an automatic pull request. Skipping build."
  echo "##teamcity[buildStop comment='Remote branch origin/$prhead has gone away; probably an automatic pull request. Skipping build.']"
  ./report-build-status.sh finish "$@"
  exit 0
fi

# Which platform are we watching?
eval watch='$'watch_$platform
# Add common patterns to the watch list
watch="^($platform|(oem/[^/]+/$platform)|resources|$watch)"

# echo "watch=$watch"

# Scan the files found 
while IFS= read -r line; do
  # echo "... $line"
  if [[ "$line" =~ $watch ]]; then
    echo "Platform $platform is impacted by changes found in PR #$PRNUM. Continuing build."
    exit 0
  fi
done <<< "$prfiles"

popd >/dev/null

# Cancel the build
echo "Platform $platform is not impacted by the changes found in PR #$PRNUM. Skipping build."
echo "##teamcity[buildStop comment='Platform $platform is not impacted by the changes found in PR #$PRNUM. Skipping build.']"
./report-build-status.sh finish "$@"
exit 0
