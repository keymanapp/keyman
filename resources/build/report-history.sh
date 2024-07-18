#!/usr/bin/env bash

set -e
set -u

# usage: report-history [--rebuild] [--base base] [--token github_token]
# where --rebuild or -r forces a build, --base or -b specifies a base

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

BUILD=false
BASE=`git branch --show-current`
GITHUB_TOKEN=
GITHUB_PR=
FROM=
FROM_VALUE=
TO=
TO_VALUE=

display_usage() {
  echo "Usage: report-history.sh [options] --token <github-token>"
  echo
  echo "  --token|-t <github-token>   Specifies a GitHub login token"
  echo "  --base|-b  <base>           Specifies branch to report on: master, beta, stable-x.y (default: master)"
  echo "  --rebuild|-r                Rebuild version.js used in this script"
  echo "  --from <version|commit>     Starting version to report from (must be on same branch)"
  echo "  --to <version|commit>       Finishing version to report to (must be on same branch)"
  echo "  --help|-?                   Show this help"
  echo "  --github-pr                 Query GitHub for Pull Request number and title instead of parsing from merge commit comments"
  echo
  echo "If --from and --to are not specified, then prints a report of outstanding changes on the branch <base> that will "
  echo "be incorporated into the next build, otherwise prints a report of all changes between those two builds."
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
    --from)
      FROM=--from
      FROM_VALUE="$2"
      shift
      ;;
    --to)
      TO=--to
      TO_VALUE="$2"
      shift
      ;;
    *)
      builder_die "Invalid parameters. Use --help for help"
  esac
  shift
done

if [ -z "$GITHUB_TOKEN" ]; then
  builder_die "Github token must be specified"
fi

if [ ! "$BASE" == "master" ] && [ ! "$BASE" == "beta" ] && [[ ! $BASE =~ stable-.+ ]]; then
  builder_die "Invalid base branch $BASE"
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
node resources/build/version/lib/index.js report-history -t "$GITHUB_TOKEN" -b "$BASE" $GITHUB_PR $FROM "$FROM_VALUE" $TO "$TO_VALUE"
popd > /dev/null
