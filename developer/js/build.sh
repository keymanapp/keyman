#!/bin/bash
#
# Compiles the developer tools, including the language model compilers.
#

# Exit on command failure and when using unset variables:
set -eu

# Include some helper functions from resources
. ../../resources/shellHelperFunctions.sh
EX_USAGE=64


# Build the main script.
build () {
  npm run build || fail "Could not build top-level JavaScript file."
}

set_version () {
  local version=$1
  # We use --no-git-tag-version because our CI system controls version numbering and
  # already tags releases. We also want to have the version of this match the
  # release of Keyman Developer -- these two versions should be in sync. Because this
  # is a large repo with multiple projects and build systems, it's better for us that
  # individual build systems don't take too much ownership of git tagging. :)
  npm --no-git-tag-version --allow-same-version version "$version" || fail "Could not set package version to $version."
}

display_usage ( ) {
  echo "Usage: $0 [-test] [-version version] [-tier tier]"
  echo "       $0 -help"
  echo
  echo "  -help               displays this screen and exits"
  echo "  -test               runs unit tests after building"
  echo "  -version version    sets the package version before building"
  echo "  -tier tier          also sets the package version tier (alpha, beta, stable) before building"
  echo "                      If version has 4 components, only first three are used."
}

################################ Main script ################################

run_tests=0
install_dependencies=1
publish_version=
publish_tier=
lastkey=

# Process command-line arguments
while [[ $# -gt 0 ]] ; do
  key="$1"
  if [[ -z "$lastkey" ]]; then
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
        lastkey=$key
        ;;
      -tier)
        lastkey=$key
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

if [ ! -z "$publish_tier" ]; then
  [ -z "$publish_version" ] && fail "-tier cannot be specified without -version"
  case "$publish_tier" in
    alpha)
      publish_tier=-alpha
      ;;
    beta)
      publish_tier=-beta
      ;;
    stable)
      # Stable releases intentionally have a blank publish tier:
      publish_tier=
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
  npm install || fail "Could not download dependencies."
fi

if [ ! -z "$publish_version" ]; then
  set_version "$publish_version" || fail "Setting version failed."
fi

build || fail "Compilation failed."
echo "Typescript compilation successful."

if (( run_tests )); then
  npm test || fail "Tests failed"
fi
