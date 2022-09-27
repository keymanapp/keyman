#!/usr/bin/env bash
#
# Packages the @keymanapp/models-types package.
#

# Exit on command failure and when using unset variables:
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
. "$REPO_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE

EX_USAGE=64


display_usage ( ) {
  echo "Usage: $0 [-test] [-publish-to-npm]"
  echo "       $0 -help"
  echo
  echo "  -help               displays this screen and exits"
  echo "  -test               runs tests"
  echo "  -publish-to-npm     publishes the current version to the npm package index"
  echo "  -dry-run            do test, etc, but don't actually publish"
}

################################ Main script ################################

run_tests=0
install_dependencies=1
should_publish=0
npm_dist_tag=
should_dry_run=0

# Process command-line arguments
while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    -help|-h)
      display_usage
      exit
      ;;
    -dry-run)
      should_dry_run=1
      ;;
    -test)
      run_tests=1
      install_dependencies=0
      ;;
    -version)
      echo "Warning: -version is now ignored"
      ;;
    -tier)
      echo "Warning: -tier is now ignored"
      ;;
    -publish-to-npm)
      should_publish=1
      ;;
    *)
      echo "$0: invalid option: $key"
      display_usage
      exit $EX_USAGE
  esac
  shift # past the processed argument
done

# Dry run settings
if (( should_dry_run )); then
  DRY_RUN=--dry-run
else
  DRY_RUN=
fi

# Check that we're doing any task at all!
if (( !run_tests )) && (( !should_publish )); then
  echo "$0: Must do at least one of: -test, -publish-npm" >&2
  display_usage
  exit $EX_USAGE
fi

# Check if Node.JS/npm is installed.
type npm >/dev/null ||\
    fail "Build environment setup error detected!  Please ensure Node.js is installed!"

if (( install_dependencies )) ; then
  npm install || fail "Could not download dependencies."
fi

if (( run_tests )); then
  npm test || fail "Tests failed"
fi

if (( should_publish )); then
  if [[ $TIER == stable ]]; then
    npm_dist_tag=latest
  else
    npm_dist_tag=$TIER
  fi

  set_npm_version

  # Note: In either case, npm publish MUST be given --access public to publish
  # a package in the @keymanapp scope on the public npm package index.
  #
  # See `npm help publish` for more details.
  echo "Publishing $DRY_RUN npm package with tag $npm_dist_tag"
  npm publish $DRY_RUN --access public --tag $npm_dist_tag || fail "Could not publish $npm_dist_tag release."
fi
