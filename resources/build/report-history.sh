#!/usr/bin/env bash

set -e
set -u

# usage: report-history [--rebuild] [--base base] [--token github_token]
# where --rebuild or -r forces a build, --base or -b specifies a base

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

BUILD=false
BASE=`git branch --show-current`
GITHUB_TOKEN=
GITHUB_PR=

display_usage() {
  echo "Usage: report-history.sh [options] --token <github-token>"
  echo
  echo "  --token|-t <github-token>   Specifies a GitHub login token"
  echo "  --base|-b  <base>           Specifies branch to report on: master, beta, stable-x.y (default: master)"
  echo "  --rebuild|-r                Rebuild version.js used in this script"
  echo "  --help|-?                   Show this help"
  echo "  --github-pr                 Query GitHub for Pull Request number and title instead of parsing from merge commit comments"
  echo
  echo "Prints a report of outstanding changes on the branch <base> that will "
  echo "be incorporated into the next build."
}

# Parse args
shopt -s nocasematch

while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    --rebuild|-r)
      BUILD=true
      ;;
    --help|-\?)
      display_usage
      exit 0
      ;;
    --token|-t)
      GITHUB_TOKEN="$2"
      shift
      ;;
    --base|-b)
      BASE="$2"
      shift
      ;;
    --github-pr)
      GITHUB_PR=$key
      ;;
    *)
      fail "Invalid parameters. Use --help for help"
  esac
  shift
done

if [ -z "$GITHUB_TOKEN" ]; then
  fail "Github token must be specified"
fi

if [ ! "$BASE" == "master" ] && [ ! "$BASE" == "beta" ] && [[ ! $BASE =~ stable-.+ ]]; then
  fail "Invalid base branch $BASE"
fi

if [ ! -f "$KEYMAN_ROOT/resources/build/version/lib/index.js" ]; then
  echo "Script not found, building..."
  BUILD=true
fi

if $BUILD; then
  pushd "$KEYMAN_ROOT/resources/build/version" > /dev/null
  npm install
  npm run build:ts
  popd > /dev/null
fi

pushd "$KEYMAN_ROOT" > /dev/null
node resources/build/version/lib/index.js report-history -t "$GITHUB_TOKEN" -b "$BASE" $GITHUB_PR
popd > /dev/null
