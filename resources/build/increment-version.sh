#!/bin/bash

set -e
set -u

#
# Usage: increment-version.sh [commit base]
#
# Increments the patch version on VERSION.md
#
# If -commit is specified, pushes the new version
# to the repository. base snould be either
# master or beta.
#

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$(dirname "$THIS_SCRIPT")/trigger-definitions.inc.sh"
. "$(dirname "$THIS_SCRIPT")/trigger-builds.inc.sh"
. "$(dirname "$THIS_SCRIPT")/sentry-control.inc.sh"

gitbranch=`git branch --show-current`

if [[ $# -gt 0 ]]; then
  # We want the action to specify the branch as a consistency check
  # for now at least.
  action=$1
  if [[ $# -gt 1 ]]; then
    base=$2
  else
    action=help
  fi

  if [[ $action == commit ]]; then
    if [ "$base" != "master" ] && [ "$base" != "beta" ]; then
      echo "Invalid branch $base specified: must be master or beta."
      exit 1
    fi

    if [ "$base" != "$gitbranch" ]; then
      echo "Branch doesn't match currently checked out branch."
      exit 2
    fi
  fi
else
  action="increment"
  base="$gitbranch"
fi

if [[ $action == help ]]; then
  echo "Usage: increment-version.sh [commit base]"
  echo "  base must be either master or beta."
  echo "  base must be equal to currently checked-out"
  echo "  branch (this may not be required in future)"
  exit 1
fi

# Let's ensure our base is up to date with GitHub to
# avoid transient errors
git pull origin $base

#
# Run the increment + history refresh
#

pushd "$KEYMAN_ROOT/resources/build/version" > /dev/null
npm install
npm run build:ts
popd > /dev/null

pushd "$KEYMAN_ROOT" > /dev/null
ABORT=0
node resources/build/version/lib/index.js history version -t "$GITHUB_TOKEN" -b "$base" || ABORT=1

if [[ $ABORT = 1 ]]; then
  echo "Skipping version increment from $VERSION: no recently merged pull requests were found"
  if [ ! -z "${TEAMCITY_VERSION-}" ]; then
    # Send TeamCity a build status
    echo "##teamcity[buildStatus status='SUCCESS' text='Skipping version increment from $VERSION: no recently merged pull requests were found']"
  fi
  exit 0
fi
popd > /dev/null

#
# Push the result
#

if [ "$action" == "commit" ]; then
  VERSION_MD="$KEYMAN_ROOT/VERSION.md"
  NEWVERSION=`cat $VERSION_MD | tr -d "[:space:]"`

  pushd "$KEYMAN_ROOT" > /dev/null
  message="auto: increment $base version to $NEWVERSION"
  branch="auto/version-$base-$NEWVERSION"
  git tag -a "release-$VERSION_WITH_TAG" -m "Keyman release $VERSION_WITH_TAG"
  git checkout -b "$branch"
  git add VERSION.md HISTORY.md

  # Also updates package versions, committing those changes
  # Uses the 'version' run script defined in the base package.json.
  npm install

  npm run version -- "$NEWVERSION"

  # Version info will be updated in lerna.json and all the lerna-managed package.json files.
  git add lerna.json
  # Iterates through all lerna-managed packages in serial mode (--concurrency 1),
  # git-adding only the updated package.json and package-lock.json files.
  npm run lerna -- exec --concurrency 1 -- git add package.json package-lock.json

  # Now that all version-related changes are ready and git-added, it's time to commit.
  git commit -m "$message"
  git push --tags origin "$branch"
  hub pull-request -f --no-edit -b $base -l auto
  git checkout $base

  #
  # Tell Sentry about this (previous version)
  #

  makeSentryRelease
  popd > /dev/null

  #
  # Trigger builds for the previous version on TeamCity and Jenkins
  #

  triggerBuilds

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

  cd "$KEYMAN_ROOT"
  git checkout "$branch"
fi

exit 0
