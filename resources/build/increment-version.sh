#!/bin/bash

set -e
set -u

#
# Usage: increment-version.sh [-commit branch]
#
# Increments the patch version on VERSION.md
#
# If -commit is specified, pushes the new version
# to the repository. branch snould be either
# master or beta.
#

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

gitbranch=`git branch --show-current`

if [[ $# -gt 1 ]] && [[ $1 == -commit ]]; then
  # We want the action to specify the branch as a consistency check
  # for now at least.
  action=$1
  branch=$2
  if [ "$branch" != "master" ] && [ "$branch" != "beta" ]; then
    echo "Invalid branch $branch specified: must be master or beta."
    exit 1
  fi

  if [ "$branch" != "$gitbranch" ]; then
    echo "Branch doesn't match currently checked out branch."
    exit 2
  fi
else
  action="increment"
  branch="$gitbranch"
fi

NEWVERSION="$VERSION_MAJOR.$VERSION_MINOR.$(($VERSION_PATCH + 1))"

echo "Incrementing $branch version from $VERSION to $NEWVERSION"
echo "$NEWVERSION" > "$KEYMAN_ROOT/VERSION.md"

if [ "$action" == "-commit" ]; then
  pushd "$KEYMAN_ROOT" > /dev/null
  message="auto: increment $branch version to $NEWVERSION"
  prbranch="auto/version-$branch-$NEWVERSION"
  git checkout -b "$prbranch"
  git add VERSION.md
  git commit -m "$message"
  git push origin "$prbranch"
  hub pull-request -f --no-edit -b $branch -l auto
  git checkout $branch
  git branch -D "$prbranch"
  popd > /dev/null
fi
