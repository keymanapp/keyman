#!/bin/bash
#
# This script sets build environment variables:
#   VERSION:          Full current build version, e.g. "14.0.1"
#   VERSION_WIN:      Full current build version for Windows, e.g. "14.0.1.0"
#   VERSION_RELEASE:  Current release version, e.g. "14.0"
#   VERSION_MAJOR:    Major version, e.g. "14"
#   VERSION_MINOR:    Minor version, e.g. "0"
#   VERSION_PATCH:    Patch version, e.g. "1"
#   TIER:             Current tier, one of "alpha", "beta" or "stable"
#   VERSION_TAG:      Tier + Pull Request + Location of build [-alpha|-beta][-test[-1234]][-local]
#   VERSION_WITH_TAG: e.g. "14.0.1-alpha-test-1234" or "14.0.5-beta-local" or "14.0.1-alpha-test"
#   KEYMAN_ROOT:      fully resolved root path of Keyman repository
#
# On macOS, this script requires coreutils (`brew install coreutils`)
#
# Here is how to include this script reliably, cross-platform:
#    ## START STANDARD BUILD SCRIPT INCLUDE
#    # adjust relative paths as necessary
#    THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
#    . "$(dirname "$THIS_SCRIPT")/../resources/build/build-utils.sh"
#    # END STANDARD BUILD SCRIPT INCLUDE
#

function die () {
    # TODO: consolidate this with fail() from shellHelperFunctions.sh
    echo
    echo "$*"
    echo
    exit 1
}

function findRepositoryRoot() {
    # See https://stackoverflow.com/questions/59895/how-to-get-the-source-directory-of-a-bash-script-from-within-the-script-itself
    # None of the answers are 100% correct for cross-platform
    # On macOS, requires coreutils (`brew install coreutils`)

    local SCRIPT=$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")
    KEYMAN_ROOT=$(dirname $(dirname $(dirname "$SCRIPT")))
    readonly KEYMAN_ROOT
}

function findVersion() {
    local VERSION_MD="$KEYMAN_ROOT/VERSION.md"
    VERSION=`cat $VERSION_MD | tr -d "[:space:]"`
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

    if [ -z "${TEAMCITY_VERSION-}" ]; then
        # Local dev machine, not TeamCity
        VERSION_TAG="$VERSION_TAG-local"
    else
        # On TeamCity; are we running a pull request build or a master/beta/stable build?
        if [ ! -z "${TEAMCITY_PR_NUMBER-}" ]; then
            # Note TEAMCITY_PR_NUMBER can also be 'master', 'beta', or 'stable-x.y'
            # This indicates we are running a Test build.
            if [[ $TEAMCITY_PR_NUMBER =~ ^(master|beta|stable(-[0-9]+\.[0-9]+)?)$ ]]; then
                VERSION_TAG="$VERSION_TAG-test"
            else
                VERSION_TAG="$VERSION_TAG-test-$TEAMCITY_PR_NUMBER"
            fi
        fi
    fi

    VERSION_WITH_TAG="$VERSION$VERSION_TAG"

    readonly VERSION
    readonly VERSION_MAJOR
    readonly VERSION_MINOR
    readonly VERSION_PATCH
    readonly VERSION_RELEASE
    readonly VERSION_WIN
    readonly VERSION_TAG
    readonly VERSION_WITH_TAG
}

function findTier() {
    local TIER_MD="$KEYMAN_ROOT/TIER.md"
    TIER=`cat $TIER_MD | tr -d "[:space:]"`
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
    echo "KEYMAN_ROOT:      $KEYMAN_ROOT"
    echo "VERSION:          $VERSION"
    echo "VERSION_WIN:      $VERSION_WIN"
    echo "VERSION_RELEASE:  $VERSION_RELEASE"
    echo "VERSION_MAJOR:    $VERSION_MAJOR"
    echo "VERSION_MINOR:    $VERSION_MINOR"
    echo "VERSION_PATCH:    $VERSION_PATCH"
    echo "TIER:             $TIER"
    echo "VERSION_TAG:      $VERSION_TAG"
    echo "VERSION_WITH_TAG: $VERSION_WITH_TAG"
}

findRepositoryRoot
findTier
findVersion
# printVersionUtilsDebug
printBuildNumberForTeamCity
