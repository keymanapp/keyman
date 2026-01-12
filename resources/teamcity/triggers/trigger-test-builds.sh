#!/usr/bin/env bash
#
# Determine if we need to do a build based on rules in
# trigger-definitions.inc.sh and based on commit trailers and PR body comments,
# rather than calculating changes in TeamCity. If a build is needed, then we ask
# TeamCity / GHA to start the build.
#

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/build/ci/trigger-definitions.inc.sh"
. "${KEYMAN_ROOT}/resources/build/ci/trigger-builds.inc.sh"
. "${KEYMAN_ROOT}/resources/build/ci/trigger-build-bot.inc.sh"
. "${KEYMAN_ROOT}/resources/build/jq.inc.sh"

builder_describe "Run test builds for the given pull request/primary branch" \
  "--dry-run,-n       Only report back which builds would be started" \
  "--branch,-b=PRNUM  Branch (master,beta,stable-x.y) or pull-request to test"

builder_parse "$@"

# Validate parameters

if ! builder_has_option --branch; then
  builder_die "--branch parameter is required"
fi

if [[ ! "${PRNUM}" =~ ^([0-9]+|master|beta|stable-[0-9]+\.[0-9]+)$ ]]; then
  builder_die "Invalid parameter --branch '${PRNUM}'; expected 'master', 'beta', 'stable-x.y', or PR number"
fi

#
# Debug logging
#
function debug_echo() {
  if [[ ! -z ${DEBUG:-} ]]; then
    echo "DEBUG: $1"
  fi
  true
}

#
# Iterate through the platforms array passed in and
# run builds associated with each platform found
#
# Parameters:
#   1: name of associative array of platforms, platform=skip|build|release|[fulltest?]
#   2: branch name or PR number
#
function triggerTestBuilds() {
  local -n platforms=$1
  local branch="$2"

  local found_build=false

  # Cancel any already running builds for this branch
  if builder_has_option --dry-run; then
    builder_echo "DRY RUN: cancel current builds for ${branch}"
  else
    node "${KEYMAN_ROOT}/resources/build/ci/cancel-builds/cancel-test-builds.mjs" "${branch}" "${TEAMCITY_TOKEN}"
  fi

  for platform in "${!platforms[@]}"; do
    local platformBuildLevel=${platforms[${platform}]}
    if [[ ${platformBuildLevel} == skip ]]; then
      builder_echo heading "${platform}: skipping build"
      continue
    fi

    builder_echo heading "${platform}: checking for git changes"
    eval test_builds='(${'bc_test_${platform}'[@]})'
    # shellcheck disable=SC2154
    for test_build in "${test_builds[@]}"; do
      if [[ ${test_build} == "" ]]; then continue; fi
      found_build=true
      if [[ "${test_build:(-7)}" == "_GitHub" ]]; then
        local job=${test_build%_GitHub}

        if builder_has_option --dry-run; then
          builder_echo "DRY RUN: Triggering GitHub action build ${job}/${branch}, level = ${platformBuildLevel}"
        else
          builder_echo "Triggering GitHub action build ${job}/${branch}, level = ${platformBuildLevel}"
          triggerGitHubActionsBuild true "${platformBuildLevel}" "${job}" "${branch}" "${KEYMAN_API_CHECK_SKIP}"
        fi
      else
        if builder_has_option --dry-run; then
          builder_echo "DRY RUN: Triggering build configuration ${test_build} on teamcity for ${branch}, level = ${platformBuildLevel}"
        else
          builder_echo "Triggering build configuration ${test_build} on teamcity for ${branch}, level = ${platformBuildLevel}"
          triggerTeamCityBuild true "${platformBuildLevel}" "${test_build}" "${vcs_test}" "${branch}"
        fi
      fi
    done
  done

  if [[ ${found_build} == false ]]; then
    postSkippedBuildsStatusResult
  fi
}

#
# Add a 'Test Build (Keyman)' successful status check to the commit
#
function postSkippedBuildsStatusResult() {
  if builder_has_option --dry-run; then
    builder_echo "DRY RUN: write successful status check 'Skipping since no platform builds necessary'"
  else
    curl --silent --write-out '\n' \
      --request POST \
      --header "Accept: application/vnd.github+json" \
      --header "Authorization: token ${GITHUB_TOKEN}" \
      --data '{"state":"success","description":"Skipping since no platform builds necessary","context":"Test Build (Keyman)"}' \
      "https://api.github.com/repos/keymanapp/keyman/statuses/${BUILD_VCS_NUMBER}"
  fi
}

#
# Find the platforms that have changes based on the watch_ variables in trigger-definitions.inc.sh
#
function find_platform_changes() {
  builder_echo grey "# Find platforms that have changes"
  declare -gA build_platforms
  local platform watch

  # Scan the files found
  while IFS= read -r line; do
    # for each platform
    echo "$line"
    for platform in "${available_platforms[@]}"; do
      if [[ ! " ${!build_platforms[*]} " =~ " ${platform} " ]]; then
        # Build a variable name for the current platform
        watch="watch_${platform}"
        # And then get the variable value
        watch="${!watch}"
        # Add common patterns to the watch list
        watch="^(${platform}|(oem/[^/]+/${platform})|resources/((?!teamcity|docker-images)|teamcity/(${platform}|includes)|docker-images/(${platform}|base))|${watch})"
        # Since bash doesn't support negative look-aheads we use perl to test
        # (grep has a --perl-regexp option, but not the version on macOS)
        if perl -e 'exit($ARGV[0] =~ /$ARGV[1]/ ? 0 : 1)' "${line}" "${watch}"; then
          # By default, we'll build a 'release' test build for touched platforms
          echo "  adding default build platform ${platform}"
          build_platforms[${platform}]=${build_level_release}
        fi
      fi
    done
  done <<< "${prfiles}"

  builder_echo blue "Default build platforms: ${!build_platforms[*]}"
}

KEYMAN_API_CHECK_SKIP="false"

#
# Following is not an error; this script can run against master/beta/stable
# branches as well as pull requests
#

if [[ ! "${PRNUM}" =~ ^[[:digit:]]+$ ]]; then
  # branch name is 'master', 'beta', or 'stable-x.y'
  builder_echo "Branch ${PRNUM} needs to pass tests on all platforms."
  triggerTestBuilds main_branch_platform_build_levels "${PRNUM}"
  exit 0
fi

#
# Find the base and head of the pull request from GitHub (while we can
# just use the current branch name, this lets us test the script against
# different pull requests; and there is no way to know which base is
# targeted by the PR other than to ask GitHub, anyway.)
#

builder_echo grey "# Get information about pull request #${PRNUM} from GitHub"
prinfo=$(curl -s -H "User-Agent: @keymanapp" -H "Authorization: Bearer ${GITHUB_TOKEN}" "https://api.github.com/repos/keymanapp/keyman/pulls/${PRNUM}")
prbase=$(echo "${prinfo}" | "${JQ}" -r '.base.ref')
prhead=$(echo "${prinfo}" | "${JQ}" -r '.head.ref')
prbaserepo=$(echo "${prinfo}" | "${JQ}" -r '.base.repo.html_url')
prheadrepo=$(echo "${prinfo}" | "${JQ}" -r '.head.repo.html_url')

debug_echo "PRNUM: ${PRNUM}"
debug_echo "BASE: ${prbase} (${prbaserepo})"
debug_echo "HEAD: ${prhead} (${prheadrepo})"

prremote=origin
if [[ "${prbaserepo}" != "https://github.com/keymanapp/keyman" ]]; then
  echo "Unknown base repository ${prbaserepo}."
  exit 1
fi

#
# By default, TeamCity does not fetch any remote branch information
# so let's grab the remote branch info before we try and run a diff
#

# (Ensure we are within the repository before running git calls)
builder_echo grey "# Ensure our local branch is up to date"
pushd "${KEYMAN_ROOT}" > /dev/null
git fetch origin > /dev/null

#
# If the repo is a fork, we need to fetch the remote as well; note that
# this only happens on a manual build as automatic trigger does not run
# on untrusted forks.
#

if [[ "${prheadrepo}" != "https://github.com/keymanapp/keyman" ]]; then
  prremote=external-rrtb
  git remote add "${prremote}" "${prheadrepo}"
  git fetch "${prremote}" > /dev/null
fi

#
# Then get a list of changed files between BASE of the branch and the branch itself
# We work from origin so we don't need the branches in our local copy
#

builder_echo grey "# Get list of changed files in the pull request"
prfiles=$(git diff --name-only "origin/${prbase}...${prremote}/${prhead}" || ( if [[ $? == 128 ]]; then echo abort; else exit $?; fi ))
if [[ "${prfiles}" == "abort" ]]; then
  # Don't trigger any builds, exit with success
  if [[ "${prremote}" != "origin" ]]; then
    git remote remove "${prremote}"
  fi
  echo "Remote branch origin/${prhead} has gone away; probably an automatic pull request. Skipping build."
  exit 0
fi

if [[ "${prremote}" != "origin" ]]; then
  git remote remove "${prremote}"
fi

echo "Found $(echo "${prfiles}" | wc -l) changed file(s)"

popd > /dev/null

find_platform_changes

#
# Now check PR commits for Build-bot: commands
# This will modify the build_platforms array
#

if [[ "${prremote}" == "origin" ]]; then
  # We only accept Build-bot commands on trusted local origin PRs
  cd "${KEYMAN_ROOT}/resources/build/ci/github"
  npm ci
  if builder_is_windows; then
    # See https://stackoverflow.com/questions/45890339/stdout-is-not-a-tty-using-bash-for-node-tape-tap-spec
    NODE_WRAPPER=node.exe
  else
    NODE_WRAPPER=node
  fi
  # Note: we may move all of this into javascript at some point!
  prcommits=$(${NODE_WRAPPER} api-pull-commits.mjs "${GITHUB_TOKEN}" "${PRNUM}")
  test_bot_check_pr_body "${PRNUM}" "${prinfo}"
  build_bot_check_messages "${PRNUM}" "${prinfo}" "${prcommits}"
  KEYMAN_API_CHECK_SKIP="$(is_skip_keyman_api_check "${PRNUM}" "${prinfo}")"
fi

#
# Start test builds as required
#

if [[ ! -z "${build_platforms[*]:-}" ]] && (( ${#build_platforms[@]} > 0)); then
  builder_echo heading "Start test builds"
  triggerTestBuilds build_platforms "${PRNUM}"
else
  builder_echo heading "No builds to start"
  postSkippedBuildsStatusResult
fi

exit 0
