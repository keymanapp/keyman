#!/usr/bin/env bash
#
# This script sets build environment variables. VERSION vars are also exported:
#   VERSION:          Full current build version, e.g. "14.0.1"
#   VERSION_WIN:      Full current build version for Windows, e.g. "14.0.1.0"
#   VERSION_RELEASE:  Current release version, e.g. "14.0"
#   VERSION_MAJOR:    Major version, e.g. "14"
#   VERSION_MINOR:    Minor version, e.g. "0"
#   VERSION_PATCH:    Patch version, e.g. "1"
#   TIER:             Current tier, one of "alpha", "beta" or "stable"
#   VERSION_TAG:      Tier + Pull Request + Location of build [-alpha|-beta][-test[-1234]][-local]
#   VERSION_WITH_TAG: e.g. "14.0.1-alpha-test-1234" or "14.0.5-beta-local" or "14.0.1-alpha-test"
#   VERSION_GIT_TAG:  Git tag for the release, "release@$VERSION_WITH_TAG", e.g. "release@14.0.1-alpha-test-1234"
#   KEYMAN_ROOT:      fully resolved root path of Keyman repository
#   VERSION_ENVIRONMENT: One of: local, test, alpha, beta, stable
#   UPLOAD_SENTRY:    true - if VERSION_ENVIRONMENT is one of alpha, beta, stable
#                     false - if local, test.  Indicates if debug artifacts should be uploaded to Sentry
#   BUILDER_OS:       win|mac|linux -- current build environment
#
# On macOS, this script requires coreutils (`brew install coreutils`)
#
# Here is how to include this script reliably, cross-platform:
#    ## START STANDARD BUILD SCRIPT INCLUDE
#    # adjust relative paths as necessary
#    THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
#    . "${THIS_SCRIPT%/*}/../resources/build/build-utils.sh"
#    # END STANDARD BUILD SCRIPT INCLUDE
#
# Note: keep changes to version, tier and tag determination in sync with mkver (windows/src/buildutils/mkver)
#

# Note: set -eu and SHLVL are deliberately set both here and in builder.inc.sh,
#       because we want them set as early as possible, and because
#       builder.inc.sh is shared to other repos, this keeps the usage consistent
#       there as well.

# Exit on command failure and when using unset variables:
set -eu

#
# Prevents 'clear' on exit of mingw64 bash shell
#
SHLVL=0

function findKeymanRoot() {
  # We don't need readlink here because our standard script prolog does a
  # readlink -f already so we will have already escaped from any symlinks
  # But we still need to canonicalize paths to remove ../../..
  KEYMAN_ROOT="${BASH_SOURCE[0]%/*/*/*}"
  KEYMAN_ROOT="$( cd "$KEYMAN_ROOT" && echo "$PWD" )"
  readonly KEYMAN_ROOT
}

function findVersion() {
    local VERSION_MD="$KEYMAN_ROOT/VERSION.md"
    VERSION=$(builder_trim $(<"$VERSION_MD"))
    [[ "$VERSION" =~ ^([[:digit:]]+)\.([[:digit:]]+)\.([[:digit:]]+)$ ]] && {
        VERSION_MAJOR="${BASH_REMATCH[1]}"
        VERSION_MINOR="${BASH_REMATCH[2]}"
        VERSION_PATCH="${BASH_REMATCH[3]}"
        VERSION_RELEASE="$VERSION_MAJOR.$VERSION_MINOR"
    } || {
        echo "Invalid VERSION.md file: expected major.minor.patch";
        exit 1;
    }

    # Used for Windows, which requires a four part version string
    VERSION_WIN="$VERSION.0"

    #
    # Build a tag to append to the version string. This is not assigned
    # to the version number used in the projects but may be used as a
    # display string and in TeamCity configuration
    #

    if [ "$TIER" == "alpha" ] || [ "$TIER" == "beta" ]; then
        VERSION_TAG="-$TIER"
    else
        VERSION_TAG=
    fi

    if [[ -z "${TEAMCITY_VERSION-}" && -z "${GITHUB_ACTIONS-}" && -z "${KEYMAN_PKG_BUILD-}" ]]; then
        # Local dev machine, not TeamCity or GitHub Action and not .deb package build
        VERSION_TAG="$VERSION_TAG-local"
        VERSION_ENVIRONMENT=local
    elif [ -n "${TEAMCITY_PR_NUMBER-}" ]; then
        # On TeamCity: are we running a pull request build or a master/beta/stable build?
        VERSION_ENVIRONMENT="test"
        # Note TEAMCITY_PR_NUMBER can also be 'master', 'beta', or 'stable-x.y'
        # This indicates we are running a Test build.
        if [[ $TEAMCITY_PR_NUMBER =~ ^(master|beta|stable(-[0-9]+\.[0-9]+)?)$ ]]; then
            VERSION_TAG="$VERSION_TAG-test"
        else
            VERSION_TAG="$VERSION_TAG-test-$TEAMCITY_PR_NUMBER"
        fi
    elif [ -n "${GITHUB_ACTIONS-}" ] && ${GHA_TEST_BUILD-}; then
        VERSION_ENVIRONMENT="test"
        # Note GHA_BRANCH can be 'master', 'beta', or 'stable-x.y'
        # This indicates we are running a Test build.
        if [[ ${GHA_BRANCH-} =~ ^(master|beta|stable(-[0-9]+\.[0-9]+)?)$ ]]; then
            VERSION_TAG="${VERSION_TAG}-test"
        else
            VERSION_TAG="${VERSION_TAG}-test-${GHA_BRANCH-unset}"
        fi
    else
        VERSION_ENVIRONMENT="$TIER"
    fi

    VERSION_WITH_TAG="$VERSION$VERSION_TAG"
    VERSION_GIT_TAG="release@$VERSION_WITH_TAG"

    readonly VERSION
    readonly VERSION_MAJOR
    readonly VERSION_MINOR
    readonly VERSION_PATCH
    readonly VERSION_RELEASE
    readonly VERSION_WIN
    readonly VERSION_TAG
    readonly VERSION_WITH_TAG
    readonly VERSION_ENVIRONMENT
    readonly VERSION_GIT_TAG

    # Export version strings so places like version.gradle can access them
    export VERSION
    export VERSION_MAJOR
    export VERSION_MINOR
    export VERSION_PATCH
    export VERSION_RELEASE
    export VERSION_WIN
    export VERSION_TAG
    export VERSION_WITH_TAG
    export VERSION_ENVIRONMENT
    export VERSION_GIT_TAG
}

function findTier() {
  local TIER_MD="$KEYMAN_ROOT/TIER.md"
  TIER=$(builder_trim $(<"$TIER_MD"))
  [[ "$TIER" =~ ^(alpha|beta|stable)$ ]] || {
      echo "Invalid TIER.md file: expected alpha, beta or stable."
      exit 1;
  }
}

function printBuildNumberForTeamCity() {
    if [ ! -z "${TEAMCITY_VERSION-}" ]; then
        if [ ! -z "${TEAMCITY_PR_NUMBER-}" ]; then
            echo "##teamcity[buildNumber '$VERSION_WITH_TAG']"
        else
            # For alpha/beta builds, for now we don't append the
            # version tag as buildNumber is used in the delivery
            # of the build version. We may improve this in the
            # future.
            echo "##teamcity[buildNumber '$VERSION']"
        fi
    fi
}

function printVersionUtilsDebug() {
    echo "KEYMAN_ROOT:         $KEYMAN_ROOT"
    echo "VERSION:             $VERSION"
    echo "VERSION_WIN:         $VERSION_WIN"
    echo "VERSION_RELEASE:     $VERSION_RELEASE"
    echo "VERSION_MAJOR:       $VERSION_MAJOR"
    echo "VERSION_MINOR:       $VERSION_MINOR"
    echo "VERSION_PATCH:       $VERSION_PATCH"
    echo "TIER:                $TIER"
    echo "VERSION_TAG:         $VERSION_TAG"
    echo "VERSION_WITH_TAG:    $VERSION_WITH_TAG"
    echo "VERSION_GIT_TAG:     $VERSION_GIT_TAG"
    echo "VERSION_ENVIRONMENT: $VERSION_ENVIRONMENT"
}

function findShouldSentryRelease() {
    # Default, for 'test' or 'local' environment, or in case $VERSION_ENVIRONMENT is improperly specified.
    # (May be overridden by -upload-sentry in supporting build scripts.)
    UPLOAD_SENTRY=false

    # Default: override to `true` for release builds.
    case $VERSION_ENVIRONMENT in
    # Actual release tiers
    alpha | beta | stable)
        UPLOAD_SENTRY=true
        ;;
    esac
}

if [[ -z ${KEYMAN_ROOT+x} ]]; then
  findKeymanRoot
fi

# Source builder_script
. "$KEYMAN_ROOT/resources/builder.inc.sh"

findTier
findVersion

# printVersionUtilsDebug
printBuildNumberForTeamCity

findShouldSentryRelease

# Sets the BUILDER_OS environment variable to linux|mac|win
#
_builder_get_operating_system() {
  declare -g BUILDER_OS
  # Default value, since it's the most general case/configuration to detect.
  BUILDER_OS=linux

  # Subject to change with future improvements.
  if [[ $OSTYPE == darwin* ]]; then
    BUILDER_OS=mac
  elif [[ $OSTYPE == msys ]]; then
    BUILDER_OS=win
  elif [[ $OSTYPE == cygwin ]]; then
    BUILDER_OS=win
  fi
  readonly BUILDER_OS
}

_builder_get_operating_system

# Intended for use with macOS-based builds, as Xcode build phase "run script"s do not have access to important
# environment variables.  Doesn't hurt to run it at other times as well.  The output file is .gitignore'd.
function exportEnvironmentDefinitionScript() {
    ENVIRONMENT_SH="$KEYMAN_ROOT/resources/environment.sh"

    # Remove old copy if it exists
    [ -f "$ENVIRONMENT_SH" ] && rm "$ENVIRONMENT_SH"

    # Documentation about the script, within the script.
    echo "# Do not edit - this is an autogenerated script.  See build/build-utils.sh for more details." >> "$ENVIRONMENT_SH"
    echo "# This file redefines critical environment variables for import to Xcode build phases." >> "$ENVIRONMENT_SH"
    echo "" >> "$ENVIRONMENT_SH"

    # Defining variables for VERSION here will leave static definitions that don't automatically update when a user
    # changes branches; some branches are 'similar enough' to not require full command-line based rebuilds.
    # We want that VERSION number to properly mirror the state of its branch during development so that it matches
    # any error reports that get logged to Sentry.
    #
    # As a result, we explicitly do NOT define VERSION or VERSION_TAG as part of ENVIRONMENT_SH.

    echo "# Required for successful dSYM upload for Sentry error reporting" >> "$ENVIRONMENT_SH"
    echo "export SENTRY_AUTH_TOKEN=${SENTRY_AUTH_TOKEN:-}" >> "$ENVIRONMENT_SH"
    echo "export SENTRY_URL=${SENTRY_URL:-}" >> "$ENVIRONMENT_SH"
    echo "export SENTRY_ORG=${SENTRY_ORG:-}" >> "$ENVIRONMENT_SH"

    # Ensure the autogenerated file may be successfully run/included by xcode-utils.sh.
    chmod +x "$ENVIRONMENT_SH"
}

# Detect if this script is running from within Xcode.  Obviously, this assumes we don't have other definitions
# for these variables... but they're set within Xcode during its runs.  As a result, they're not the wisest thing for
# someone else to intentionally use, so this check seems reasonable.
#
# https://gist.github.com/gdavis/6670468 has a representative copy of a standard Xcode environment variable setup.
if [ "$BUILDER_OS" == "mac" ] && [[ -z "${XCODE_VERSION_ACTUAL:-}" ]] && [[ -z "${XCODE_PRODUCT_BUILD_VERSION:-}" ]]; then
    exportEnvironmentDefinitionScript
fi


replaceVersionStrings() {
  local infile=$1
  local outfile=$2

  sed "
    s/\$VERSION_WIN/$VERSION_WIN/g;
    s/\$VERSION_RELEASE/$VERSION_RELEASE/g;
    s/\$VERSION_MAJOR/$VERSION_MAJOR/g;
    s/\$VERSION_MINOR/$VERSION_MINOR/g;
    s/\$VERSION_PATCH/$VERSION_PATCH/g;
    s/\$TIER/$TIER/g;
    s/\$VERSION_TAG/$VERSION_TAG/g;
    s/\$VERSION_WITH_TAG/$VERSION_WITH_TAG/g;
    s/\$VERSION_GIT_TAG/$VERSION_GIT_TAG/g;
    s/\$VERSION_ENVIRONMENT/$VERSION_ENVIRONMENT/g;
    s/\$VERSION/$VERSION/g;
    " "$infile" > "$outfile"
}

replaceVersionStrings_Mkver() {
  # This is similar to replaceVersionStrings but supports all
  # the old mkver strings as used by windows/src
  local infile=$1
  local outfile=$2

  # Note that $VERSION differs between the two functions!
  # We should be deprecating all the mkver strings

  sed "
    s/\$VersionWin/$VERSION_WIN/g;
    s/\$VersionRelease/$VERSION_RELEASE/g;
    s/\$VersionMajor/$VERSION_MAJOR/g;
    s/\$VersionMinor/$VERSION_MINOR/g;
    s/\$VersionPatch/$VERSION_PATCH/g;
    s/\$Tier/$TIER/g;
    s/\$Tag/$VERSION_TAG/g;
    s/\$VersionWithTag/$VERSION_WITH_TAG/g;
    s/\$VersionGitTag/$VERSION_GIT_TAG/g;
    s/\$VersionRc/$VERSION_MAJOR,$VERSION_MINOR,$VERSION_PATCH,0/g;
    s/\$Environment/$VERSION_ENVIRONMENT/g;
    s/\$Version/$VERSION/g;
    s/\$VERSIONNUM/$VERSION_MAJOR,$VERSION_MINOR,$VERSION_PATCH,0/g;
    s/\$RELEASE_MAJOR/$VERSION_MAJOR/g;
    s/\$RELEASE_MINOR/$VERSION_MINOR/g;
    s/\$RELEASE/$VERSION_RELEASE/g;

    s/\$VERSION_WIN/$VERSION_WIN/g;
    s/\$VERSION_RELEASE/$VERSION_RELEASE/g;
    s/\$VERSION_MAJOR/$VERSION_MAJOR/g;
    s/\$VERSION_MINOR/$VERSION_MINOR/g;
    s/\$VERSION_PATCH/$VERSION_PATCH/g;
    s/\$TIER/$TIER/g;
    s/\$VERSION_TAG/$VERSION_TAG/g;
    s/\$VERSION_WITH_TAG/$VERSION_WITH_TAG/g;
    s/\$VERSION_GIT_TAG/$VERSION_GIT_TAG/g;
    s/\$VERSION_ENVIRONMENT/$VERSION_ENVIRONMENT/g;

    s/\$VERSION/$VERSION_WIN/g;

    " "$infile" > "$outfile"
}

set_keyman_standard_build_path() {
  PATH="$KEYMAN_ROOT/node_modules/.bin:$PATH"
}

# For CI compatbility of building Keyman for Android 16.0 with OpenJDK 8,
# this overrides JAVA_HOME for the builder script to use OpenJDK 11.
set_java_home() {
  if [[ ! -z ${JAVA_HOME_11+x} ]]; then
    builder_echo "Setting JAVA_HOME to JAVA_HOME_11 ($JAVA_HOME_11)"
    export JAVA_HOME="${JAVA_HOME_11}"
  fi
}

#
# printXCodeBuildScriptLogs: xcodebuild does not emit stdout from scripts in
# PBXShellScriptBuildPhase phases. This is a real problem for us because if
# there is an issue, we just can't see it. So we capture the output in a
# separate logfile, and then call printXCodeBuildScriptLogs after any xcodebuild
# call to get the output.
#
# This file is captured in xcode-utils.sh, logScriptsToFile function, and each
# script phase will append to the log file, until funprintXCodeBuildScriptLogs
# is called, at which point the logfile will be deleted.
#
# The logfile is placed in $KEYMAN_ROOT/xcodebuild-scripts.log. It is used for
# both iOS and macOS builds.
#
# If there is no logfile, then this function will not emit anything.
#
printXCodeBuildScriptLogs() {
  local SCRIPT_LOG="$KEYMAN_ROOT/xcodebuild-scripts.log"
  if [ -f "$SCRIPT_LOG" ]; then
    echo "printXCodeBuildScriptLogs: reporting script results from previous xcode build"
    cat "$SCRIPT_LOG"
    rm "$SCRIPT_LOG"
    echo "printXCodeBuildScriptLogs: done"
    echo
  fi
}

#
# Wraps xcodebuild with error handling and log printing
#
run_xcodebuild() {
  typeset cmnd="$*"
  typeset ret_code
  local hasSetErrExit=false
  if [ -o errexit ]; then
    hasSetErrExit=true
    set +e
  fi
  eval xcodebuild $cmnd
  ret_code=$?
  if $hasSetErrExit; then
    set -e
  fi

  printXCodeBuildScriptLogs
  if [ $ret_code != 0 ]; then
    builder_die "Build failed! Error: [$ret_code] when executing command: 'xcodebuild $cmnd'"
  fi
}


#
# We always want to use tools out of node_modules/.bin to guarantee that we get the
# correct version
#
set_keyman_standard_build_path
