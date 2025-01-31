#!/usr/bin/env bash

set -e
set -u

#
# Usage: increment-version.sh [-f] [commit base]
#
# Increments the patch version on VERSION.md
#
# If -f is specified, triggers a build even with
# no detected changes.
#
# If commit is specified, pushes the new version
# to the repository. base snould be either
# master or beta.
#

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "${THIS_SCRIPT%/*}/trigger-definitions.inc.sh"
. "${THIS_SCRIPT%/*}/trigger-builds.inc.sh"
. "${THIS_SCRIPT%/*}/sentry-control.inc.sh"

gitbranch=`git branch --show-current`

FORCE=0
HISTORY_FORCE=
fromversion=
toversion=

if [[ $# -gt 0 ]]; then
  if [[ "$1" == "-f" ]]; then
    FORCE=1
    HISTORY_FORCE=--force
    shift
  fi
fi

if [[ $# -gt 0 ]]; then
  # We want the action to specify the branch as a consistency check
  # for now at least.
  action=$1
  if [[ $# -gt 1 ]]; then
    base=$2
    shift
  else
    action=help
  fi
  shift

  if [[ $action == commit ]]; then
    if [ "$base" != "master" ] && [ "$base" != "beta" ] && [[ ! "$base" =~ ^stable-[0-9]+\.[0-9]+$ ]]; then
      echo "Invalid branch $base specified: must be master, beta, or stable-x.y."
      exit 1
    fi

    if [ "$base" != "$gitbranch" ]; then
      echo "Branch doesn't match currently checked out branch."
      exit 2
    fi

    # commit action parameters
    if [[ $# -gt 0 ]]; then
      # next two parameters are --from and --to
      fromversion="$1"
      toversion="$2"
    fi
  fi
else
  action="increment"
  base="$gitbranch"
fi

if [[ $action == help ]]; then
  echo "Usage: increment-version.sh [-f] [commit base]"
  echo "  -f  forces a build even with no changes"
  echo "  base must be either master, beta or stable-x.y."
  echo "  base must be equal to currently checked-out"
  echo "  branch (this may not be required in future)"
  exit 1
fi

# Let's ensure our base is up to date with GitHub to
# avoid transient errors
echo "increment-version.sh: updating branch $base from GitHub"
git pull origin $base

#
# Run the increment + history refresh
#

echo "increment-version.sh: building resources/build/version"
pushd "$KEYMAN_ROOT"
npm ci

"$KEYMAN_ROOT/resources/build/version/build.sh"

echo "increment-version.sh: running resources/build/version"
pushd "$KEYMAN_ROOT"
ABORT=0
if [[ -z "$fromversion" ]]; then
  node resources/build/version/build/src/index.js history version -t "$GITHUB_TOKEN" -b "$base" $HISTORY_FORCE --github-pr || ABORT=$?
else
  node resources/build/version/build/src/index.js history version -t "$GITHUB_TOKEN" -b "$base" $HISTORY_FORCE --github-pr --from "$fromversion" --to "$toversion" || ABORT=$?
fi

if [[ $ABORT = 50 ]]; then
  if [[ $FORCE = 0 ]]; then
    echo "Skipping version increment from $VERSION: no recently merged pull requests were found"
    if [ ! -z "${TEAMCITY_VERSION-}" ]; then
      # Send TeamCity a build status
      echo "##teamcity[buildStatus status='SUCCESS' text='Skipping version increment from $VERSION: no recently merged pull requests were found']"
    fi
    exit 0
  else
    echo "Force specified; building even though no changes were detected"
  fi
elif [[ $ABORT != 0 ]]; then
  echo "Failed to complete version history check (node version/lib/index.js failed with error $ABORT)"
  exit $ABORT
fi
popd > /dev/null

#
# Push the result
#

if [ "$action" == "commit" ]; then
  echo "increment-version.sh: committing to repository and tagging release version $VERSION_WITH_TAG"
  VERSION_MD="$KEYMAN_ROOT/VERSION.md"
  NEWVERSION=`cat $VERSION_MD | tr -d "[:space:]"`

  pushd "$KEYMAN_ROOT" > /dev/null
  message="auto: increment $base version to $NEWVERSION"
  branch="auto/version-$base-$NEWVERSION"
  git tag -a "$VERSION_GIT_TAG" -m "Keyman release $VERSION_WITH_TAG"
  git checkout -b "$branch"
  git add VERSION.md HISTORY.md

  # Now that all version-related changes are ready and git-added, it's time to commit.
  git commit -m "$message"
  git push --tags origin "$branch"
  git checkout $base

  #
  # Tell Sentry about this (previous version)
  #

  makeSentryRelease
  popd > /dev/null

  #
  # Trigger builds for the previous version on TeamCity and GitHub
  #

  triggerBuilds

  #
  # Now, create the PR on GitHub which will be merged when ready
  #

  cd "$KEYMAN_ROOT"
  git checkout "$branch"
  hub pull-request -f --no-edit -b $base -l auto

  #
  # If we are on a stable-x.y branch, then we also want to merge changes to
  # HISTORY.md to master. We don't want to do this for beta or alpha builds;
  # beta changes will be merged to master periodically anyway.
  #
  if [[ "$base" =~ ^stable-[0-9]+\.[0-9]+$ ]]; then
    git switch master

    # In order to avoid potential git conflicts, we run the history collater
    # again on the master HISTORY.md. Note that the script always exits 1 to
    # indicate it hasn't updated VERSION.md. We could tweak that in the future.
    node resources/build/version/build/src/index.js history --no-write-github-comment --github-pr -t "$GITHUB_TOKEN" -b "$base" || true

    # If HISTORY.md has been updated, then we want to create a branch and push
    # it for review
    if git status --porcelain=v1 | grep -q HISTORY.md; then
      # TODO: once we are sure this is stable, rename this to
      # "$branch-master-history" to get automatic merges with "auto/..." branch
      # name
      git switch -c "chore/version-$base-$NEWVERSION-master-history" master
      git add HISTORY.md
      git commit -m "$message (history cherry-pick to master)"
      # TODO: once we are sure this is stable, add `-l auto` to get the "auto:"
      # label
      hub pull-request -f --no-edit -b master
    fi

    # Return to our best working branch
    git switch "$branch"
  fi

  #
  # Done
  #

  echo "Version was incremented and pull request was created, and builds were triggered"
  if [ ! -z "${TEAMCITY_VERSION-}" ]; then
    # Send TeamCity a build status
    echo "##teamcity[buildStatus status='SUCCESS' text='Version was incremented from $VERSION to $NEWVERSION and pull request was created']"
  fi

  #
  # After this script finishes in CI, we will be pushing the new history to
  # downloads.keyman.com, so we need to finish with the branch here that has the
  # latest history in it. We don't need to cleanup the branch because the CI will
  # do that for us.
  #
fi

exit 0
