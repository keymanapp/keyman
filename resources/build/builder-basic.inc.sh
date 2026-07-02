#!/usr/bin/env bash
#
# This script sets build environment variables. KEYMAN_VERSION vars are also exported:
#   KEYMAN_VERSION:          Full current build version, e.g. "14.0.1"
#   KEYMAN_VERSION_WIN:      Full current build version for Windows, e.g. "14.0.1.0"
#   KEYMAN_VERSION_RELEASE:  Current release version, e.g. "14.0"
#   KEYMAN_VERSION_MAJOR:    Major version, e.g. "14"
#   KEYMAN_VERSION_MINOR:    Minor version, e.g. "0"
#   KEYMAN_VERSION_PATCH:    Patch version, e.g. "1"
#   KEYMAN_TIER:             Current tier, one of "alpha", "beta" or "stable"
#   KEYMAN_VERSION_TAG:      Tier + Pull Request + Location of build [-alpha|-beta][-test[-1234]][-local]
#   KEYMAN_VERSION_WITH_TAG: e.g. "14.0.1-alpha-test-1234" or "14.0.5-beta-local" or "14.0.1-alpha-test"
#   KEYMAN_VERSION_GIT_TAG:  Git tag for the release, "release@$KEYMAN_VERSION_WITH_TAG", e.g. "release@14.0.1-alpha-test-1234"
#   KEYMAN_ROOT:      fully resolved root path of Keyman repository
#   KEYMAN_VERSION_ENVIRONMENT: One of: local, test, alpha, beta, stable
#   KEYMAN_VERSION_FOR_FILENAME: KEYMAN_VERSION, for release builds, or KEYMAN_VERSION_WITH_TAG, for PR and local builds; see #10521
#   UPLOAD_SENTRY:    true - if KEYMAN_VERSION_ENVIRONMENT is one of alpha, beta, stable
#                     false - if local, test.  Indicates if debug artifacts should be uploaded to Sentry
#   BUILDER_OS:       win|mac|linux -- current build environment
#
# On macOS, this script requires coreutils (`brew install coreutils`)
#
# Here is how to include this script reliably, cross-platform:
#    ## START STANDARD BUILD SCRIPT INCLUDE
#    # adjust relative paths as necessary
#    THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
#    . "${THIS_SCRIPT%/*}/../resources/build/builder-basic.inc.sh"
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

function _builder_basic_find_keyman_root() {
  # We don't need readlink here because our standard script prolog does a
  # readlink -f already so we will have already escaped from any symlinks but we
  # still need to canonicalize paths to remove ../../..
  #
  # We only want to set KEYMAN_ROOT if it isn't already set and readonly
  # (https://stackoverflow.com/a/4441178/1836776)
  if (unset KEYMAN_ROOT 2>/dev/null); then
    KEYMAN_ROOT="${BASH_SOURCE[0]%/*/*/*}"
    KEYMAN_ROOT="$( cd "$KEYMAN_ROOT" && echo "$PWD" )"
    readonly KEYMAN_ROOT
    export KEYMAN_ROOT
  fi
}

function _builder_basic_find_version() {
    local KEYMAN_VERSION_MD="$KEYMAN_ROOT/VERSION.md"
    KEYMAN_VERSION=$(builder_trim $(<"$KEYMAN_VERSION_MD"))
    [[ "$KEYMAN_VERSION" =~ ^([[:digit:]]+)\.([[:digit:]]+)\.([[:digit:]]+)$ ]] && {
        KEYMAN_VERSION_MAJOR="${BASH_REMATCH[1]}"
        KEYMAN_VERSION_MINOR="${BASH_REMATCH[2]}"
        KEYMAN_VERSION_PATCH="${BASH_REMATCH[3]}"
        KEYMAN_VERSION_RELEASE="$KEYMAN_VERSION_MAJOR.$KEYMAN_VERSION_MINOR"
    } || {
        echo "Invalid VERSION.md file: expected major.minor.patch";
        exit 1;
    }

    # Used for Windows, which requires a four part version string
    KEYMAN_VERSION_WIN="$KEYMAN_VERSION.0"

    #
    # Build a tag to append to the version string. This is not assigned
    # to the version number used in the projects but may be used as a
    # display string and in TeamCity configuration
    #

    if [ "$KEYMAN_TIER" == "alpha" ] || [ "$KEYMAN_TIER" == "beta" ]; then
        KEYMAN_VERSION_TAG="-$KEYMAN_TIER"
    else
        KEYMAN_VERSION_TAG=
    fi

    if ! builder_is_running_on_teamcity && ! builder_is_running_on_gha \
      && [[ -z "${KEYMAN_PKG_BUILD-}" ]]; then
        # Local dev machine, not TeamCity or GitHub Action and not .deb package build
        KEYMAN_VERSION_TAG="$KEYMAN_VERSION_TAG-local"
        KEYMAN_VERSION_ENVIRONMENT=local
    elif [ -n "${TEAMCITY_PR_NUMBER-}" ]; then
        # On TeamCity: are we running a pull request build or a master/beta/stable build?
        KEYMAN_VERSION_ENVIRONMENT="test"
        # Note TEAMCITY_PR_NUMBER can also be 'master', 'beta', or 'stable-x.y'
        # This indicates we are running a Test build.
        if [[ $TEAMCITY_PR_NUMBER =~ ^(master|beta|stable(-[0-9]+\.[0-9]+)?)$ ]]; then
            KEYMAN_VERSION_TAG="$KEYMAN_VERSION_TAG-test"
        else
            KEYMAN_VERSION_TAG="$KEYMAN_VERSION_TAG-test-$TEAMCITY_PR_NUMBER"
        fi
    elif builder_is_running_on_gha && ${GHA_TEST_BUILD-}; then
        KEYMAN_VERSION_ENVIRONMENT="test"
        # Note GHA_BRANCH can be 'master', 'beta', or 'stable-x.y'
        # This indicates we are running a Test build.
        if [[ ${GHA_BRANCH-} =~ ^(master|beta|stable(-[0-9]+\.[0-9]+)?)$ ]]; then
            KEYMAN_VERSION_TAG="${KEYMAN_VERSION_TAG}-test"
        else
            KEYMAN_VERSION_TAG="${KEYMAN_VERSION_TAG}-test-${GHA_BRANCH-unset}"
        fi
    else
        KEYMAN_VERSION_ENVIRONMENT="$KEYMAN_TIER"
    fi

    KEYMAN_VERSION_WITH_TAG="$KEYMAN_VERSION$KEYMAN_VERSION_TAG"
    KEYMAN_VERSION_GIT_TAG="release@$KEYMAN_VERSION_WITH_TAG"

    # For local builds, and for PR builds, we will use a filename including the tag
    # but for release builds, we omit the tag for now. In the future, we may include
    # the tag but that involves changing a number of other websites as dealing with
    # the rename there too.
    if builder_is_ci_release_build; then
      KEYMAN_VERSION_FOR_FILENAME="${KEYMAN_VERSION}"
    else
      KEYMAN_VERSION_FOR_FILENAME="${KEYMAN_VERSION_WITH_TAG}"
    fi

    readonly KEYMAN_VERSION
    readonly KEYMAN_VERSION_MAJOR
    readonly KEYMAN_VERSION_MINOR
    readonly KEYMAN_VERSION_PATCH
    readonly KEYMAN_VERSION_RELEASE
    readonly KEYMAN_VERSION_WIN
    readonly KEYMAN_VERSION_TAG
    readonly KEYMAN_VERSION_WITH_TAG
    readonly KEYMAN_VERSION_ENVIRONMENT
    readonly KEYMAN_VERSION_GIT_TAG
    readonly KEYMAN_VERSION_FOR_FILENAME

    # Export version strings so places like version.gradle can access them
    export KEYMAN_VERSION
    export KEYMAN_VERSION_MAJOR
    export KEYMAN_VERSION_MINOR
    export KEYMAN_VERSION_PATCH
    export KEYMAN_VERSION_RELEASE
    export KEYMAN_VERSION_WIN
    export KEYMAN_VERSION_TAG
    export KEYMAN_VERSION_WITH_TAG
    export KEYMAN_VERSION_ENVIRONMENT
    export KEYMAN_VERSION_GIT_TAG
    export KEYMAN_VERSION_FOR_FILENAME
}

function _builder_basic_find_tier() {
  local KEYMAN_TIER_MD="$KEYMAN_ROOT/TIER.md"
  KEYMAN_TIER=$(builder_trim $(<"$KEYMAN_TIER_MD"))
  [[ "$KEYMAN_TIER" =~ ^(alpha|beta|stable)$ ]] || {
      echo "Invalid TIER.md file: expected alpha, beta or stable."
      exit 1;
  }
}

function _builder_basic_print_build_number_for_teamcity() {
    if [ ! -z "${TEAMCITY_VERSION-}" ]; then
        if [ ! -z "${TEAMCITY_PR_NUMBER-}" ]; then
            echo "##teamcity[buildNumber '$KEYMAN_VERSION_WITH_TAG']"
        else
            # For alpha/beta builds, for now we don't append the
            # version tag as buildNumber is used in the delivery
            # of the build version. We may improve this in the
            # future.
            echo "##teamcity[buildNumber '$KEYMAN_VERSION']"
        fi
    fi
}

function _builder_basic_print_version_utils_debug() {
    echo "KEYMAN_ROOT:                    $KEYMAN_ROOT"
    echo "KEYMAN_VERSION:                 $KEYMAN_VERSION"
    echo "KEYMAN_VERSION_WIN:             $KEYMAN_VERSION_WIN"
    echo "KEYMAN_VERSION_RELEASE:         $KEYMAN_VERSION_RELEASE"
    echo "KEYMAN_VERSION_MAJOR:           $KEYMAN_VERSION_MAJOR"
    echo "KEYMAN_VERSION_MINOR:           $KEYMAN_VERSION_MINOR"
    echo "KEYMAN_VERSION_PATCH:           $KEYMAN_VERSION_PATCH"
    echo "KEYMAN_TIER:                    $KEYMAN_TIER"
    echo "KEYMAN_VERSION_TAG:             $KEYMAN_VERSION_TAG"
    echo "KEYMAN_VERSION_WITH_TAG:        $KEYMAN_VERSION_WITH_TAG"
    echo "KEYMAN_VERSION_GIT_TAG:         $KEYMAN_VERSION_GIT_TAG"
    echo "KEYMAN_VERSION_ENVIRONMENT:     $KEYMAN_VERSION_ENVIRONMENT"
    echo "KEYMAN_VERSION_FOR_FILENAME:    $KEYMAN_VERSION_FOR_FILENAME"
}

# TODO: consolidate with buildLevel, see #14285
function _builder_basic_find_should_sentry_release() {
    # Default, for 'test' or 'local' environment, or in case
    # $KEYMAN_VERSION_ENVIRONMENT is improperly specified.
    # (May be overridden by -upload-sentry in supporting build scripts.)
    UPLOAD_SENTRY=false

    # Default: override to `true` for release builds.
    case $KEYMAN_VERSION_ENVIRONMENT in
    # Actual release tiers
    alpha | beta | stable)
        UPLOAD_SENTRY=true
        ;;
    esac
}

replaceVersionStrings() {
  local infile=$1
  local outfile=$2

  sed "
    s/\$KEYMAN_VERSION_WIN/$KEYMAN_VERSION_WIN/g;
    s/\$KEYMAN_VERSION_RELEASE/$KEYMAN_VERSION_RELEASE/g;
    s/\$KEYMAN_VERSION_MAJOR/$KEYMAN_VERSION_MAJOR/g;
    s/\$KEYMAN_VERSION_MINOR/$KEYMAN_VERSION_MINOR/g;
    s/\$KEYMAN_VERSION_PATCH/$KEYMAN_VERSION_PATCH/g;
    s/\$KEYMAN_TIER/$KEYMAN_TIER/g;
    s/\$KEYMAN_VERSION_TAG/$KEYMAN_VERSION_TAG/g;
    s/\$KEYMAN_VERSION_WITH_TAG/$KEYMAN_VERSION_WITH_TAG/g;
    s/\$KEYMAN_VERSION_GIT_TAG/$KEYMAN_VERSION_GIT_TAG/g;
    s/\$KEYMAN_VERSION_ENVIRONMENT/$KEYMAN_VERSION_ENVIRONMENT/g;
    s/\$KEYMAN_VERSION/$KEYMAN_VERSION/g;
    s/\$KEYMAN_VERSION_FOR_FILENAME/$KEYMAN_VERSION_FOR_FILENAME/g;
    " "$infile" > "$outfile"
}

replaceVersionStrings_Mkver() {
  # This is similar to replaceVersionStrings but supports all
  # the old mkver strings as used by windows/src
  local infile=$1
  local outfile=$2

  # Note that $KEYMAN_VERSION differs between the two functions!
  # We should be deprecating all the mkver strings

  sed "
    s/\$VersionWin/$KEYMAN_VERSION_WIN/g;
    s/\$VersionRelease/$KEYMAN_VERSION_RELEASE/g;
    s/\$VersionMajor/$KEYMAN_VERSION_MAJOR/g;
    s/\$VersionMinor/$KEYMAN_VERSION_MINOR/g;
    s/\$VersionPatch/$KEYMAN_VERSION_PATCH/g;
    s/\$Tier/$KEYMAN_TIER/g;
    s/\$Tag/$KEYMAN_VERSION_TAG/g;
    s/\$VersionWithTag/$KEYMAN_VERSION_WITH_TAG/g;
    s/\$VersionGitTag/$KEYMAN_VERSION_GIT_TAG/g;
    s/\$VersionRc/$KEYMAN_VERSION_MAJOR,$KEYMAN_VERSION_MINOR,$KEYMAN_VERSION_PATCH,0/g;
    s/\$Environment/$KEYMAN_VERSION_ENVIRONMENT/g;
    s/\$Version/$KEYMAN_VERSION/g;
    s/\$KEYMAN_VERSIONNUM/$KEYMAN_VERSION_MAJOR,$KEYMAN_VERSION_MINOR,$KEYMAN_VERSION_PATCH,0/g;
    s/\$RELEASE_MAJOR/$KEYMAN_VERSION_MAJOR/g;
    s/\$RELEASE_MINOR/$KEYMAN_VERSION_MINOR/g;
    s/\$RELEASE/$KEYMAN_VERSION_RELEASE/g;

    s/\$KEYMAN_VERSION_WIN/$KEYMAN_VERSION_WIN/g;
    s/\$KEYMAN_VERSION_RELEASE/$KEYMAN_VERSION_RELEASE/g;
    s/\$KEYMAN_VERSION_MAJOR/$KEYMAN_VERSION_MAJOR/g;
    s/\$KEYMAN_VERSION_MINOR/$KEYMAN_VERSION_MINOR/g;
    s/\$KEYMAN_VERSION_PATCH/$KEYMAN_VERSION_PATCH/g;
    s/\$KEYMAN_TIER/$KEYMAN_TIER/g;
    s/\$KEYMAN_VERSION_TAG/$KEYMAN_VERSION_TAG/g;
    s/\$KEYMAN_VERSION_WITH_TAG/$KEYMAN_VERSION_WITH_TAG/g;
    s/\$KEYMAN_VERSION_GIT_TAG/$KEYMAN_VERSION_GIT_TAG/g;
    s/\$KEYMAN_VERSION_ENVIRONMENT/$KEYMAN_VERSION_ENVIRONMENT/g;
    s/\$KEYMAN_VERSION_FOR_FILENAME/$KEYMAN_VERSION_FOR_FILENAME/g;

    s/\$KEYMAN_VERSION/$KEYMAN_VERSION_WIN/g;

    " "$infile" > "$outfile"
}

_builder_basic_set_keyman_standard_build_path() {
  PATH="$KEYMAN_ROOT/node_modules/.bin:$PATH"
}

_builder_basic_find_keyman_root

# Source builder_script
. "$KEYMAN_ROOT/resources/builder.inc.sh"

_builder_basic_find_tier
_builder_basic_find_version

# _builder_basic_print_version_utils_debug
_builder_basic_print_build_number_for_teamcity

_builder_basic_find_should_sentry_release

#
# We always want to use tools out of node_modules/.bin to guarantee that we get the
# correct version
#
_builder_basic_set_keyman_standard_build_path
