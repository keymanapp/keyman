#!/bin/bash
#
# Packages the @keymanapp/models-types package.
#

# Exit on command failure and when using unset variables:
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
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

# Process command-line arguments
while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    -help|-h)
      display_usage
      exit
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

publish_version=`cat "$KEYMAN_ROOT/VERSION.md"`
publish_tier=`cat "$KEYMAN_ROOT/TIER.md"`

# Validate the publish_version
if [ ! -z "$publish_version" ]; then
  # Remove final component if more than 3 components passed
  publish_version=` echo "$publish_version" | cut -d "." -f1,2,3 -`
  [[ $publish_version =~ ^([0-9]+)\.([0-9]+)\.([0-9]+)$ ]] || fail "-version must be dotted numeric string, e.g. 1.2.3."
fi

if [ -n "$publish_tier" ]; then
  # Make sure we're either setting the version or publishing:
  if [ -z "$publish_version" ] && (( !should_publish )) ; then
    fail "-tier cannot be specified without -version or -publish-to-npm"
  fi

  case "$publish_tier" in
    alpha)
      publish_tier=-alpha
      npm_dist_tag=alpha
      ;;
    beta)
      publish_tier=-beta
      npm_dist_tag=beta
      ;;
    stable)
      # Stable releases intentionally have a blank publish tier:
      publish_tier=
      npm_dist_tag=latest
      ;;
    *)
      fail "-tier must be one of alpha, beta or stable"
  esac
  publish_version=$publish_version$publish_tier
fi

# Check that we're doing any task at all!
if (( !run_tests )) && (( !should_publish )) && [ -z "$publish_version" ]; then
  echo "$0: Must do at least one of: -test, -version, -publish-npm" >&2
  display_usage
  exit $EX_USAGE
fi

# Check if Node.JS/npm is installed.
type npm >/dev/null ||\
    fail "Build environment setup error detected!  Please ensure Node.js is installed!"

if (( install_dependencies )) ; then
  npm install || fail "Could not download dependencies."
fi

if [ -n "$publish_version" ]; then
  set_npm_version "$publish_version" || fail "Setting version failed."
fi

if (( run_tests )); then
  npm test || fail "Tests failed"
fi

if (( should_publish )); then
  # Note: In either case, npm publish MUST be given --access public to publish
  # a package in the @keymanapp scope on the public npm package index.
  #
  # See `npm help publish` for more details.
  npm publish $DRY_RUN --access public --tag "${npm_dist_tag:=latest}" || fail "Could not publish ${npm_dist_tag} release."
fi
