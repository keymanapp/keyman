#!/bin/bash
#
# Compiles the developer tools, including the language model compilers.
#

# Exit on command failure and when using unset variables:
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE
EX_USAGE=64

# Where to find lexical model types.
LEXICAL_MODELS_TYPES=../../common/models/types


# Build the main script.
build () {
  npm run build || fail "Could not build top-level JavaScript file."
}

display_usage ( ) {
  echo "Usage: $0 [-test] [-version version] [-tier tier]"
  echo "       $0 -help"
  echo
  echo "  -help               displays this screen and exits"
  echo "  -version version    sets the package version before building"
  echo "  -test               runs unit tests after building"
  echo "  -tdd                runs unit tests WITHOUT building"
  echo "  -publish-to-npm     publishes the current version to the npm package index"
  echo "  -tier tier          also sets the package version tier and npm tag (alpha, beta, stable) before building or publishing"
  echo "                      If version has 4 components, only first three are used."
}

################################ Main script ################################

run_tests=0
install_dependencies=1
publish_version=
publish_tier=
lastkey=
should_publish=0
npm_dist_tag=

# Process command-line arguments
while [[ $# -gt 0 ]] ; do
  key="$1"
  if [[ -z "$lastkey" ]]; then
    case $key in
      -help|-h)
        display_usage
        exit
        ;;
      -skip-package-install|-S)
        install_dependencies=0
        ;;
      -test)
        run_tests=1
        ;;
      -tdd)
        run_tests=1
        install_dependencies=0
        ;;
      -version)
        lastkey=$key
        ;;
      -tier)
        lastkey=$key
        ;;
      -publish-to-npm)
        should_publish=1
        ;;
      *)
        echo "$0: invalid option: $key"
        display_usage
        exit $EX_USAGE
    esac
  else
    case $lastkey in
      -version)
        publish_version=$key
        ;;
      -tier)
        publish_tier=$key
        ;;
      *)
        # Should be impossible to reach ;-)
        echo "$0: invalid option: $lastkey"
        display_usage
        exit $EX_USAGE
    esac
    lastkey=
  fi
  shift # past the processed argument
done

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

# Check if Node.JS/npm is installed.
type npm >/dev/null ||\
    fail "Build environment setup error detected!  Please ensure Node.js is installed!"

if (( install_dependencies )) ; then
  verify_npm_setup
fi

if [ -n "$publish_version" ]; then
  set_npm_version "$publish_version" || fail "Setting version failed."
fi

build || fail "Compilation failed."
echo "Typescript compilation successful."

if (( run_tests )); then
  npm test || fail "Tests failed"
fi

if (( should_publish )); then
  # Note: In either case, npm publish MUST be given --access public to publish
  # a package in the @keymanapp scope on the public npm package index.
  #
  # See `npm help publish` for more details.
  npm publish --access public --tag "${npm_dist_tag:=latest}" || fail "Could not publish ${npm_dist_tag} release."
fi
