#!/bin/bash
# Uses the open-source `wget` utility to create an embedding-friendly offline mirror
# equivalent of online help.

# Set sensible script defaults:
# set -e: Terminate script if a command returns an error
set -e
# set -u: Terminate script if an unset variable is used
set -u
# set -x: Debugging use, print each statement
# set -x

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

display_usage ( ) {
    echo "build-help.sh [-local]"
    echo
    echo "Get an offline mirror of the VERSION_RELEASE (#.0) of Keyman for Android help"
    echo "from help.keyman.com."
    echo "For alpha and beta tiers, use http://help.keyman-staging.com"
    echo "For stable tier, uses http://help.keyman.com"
    echo "  -local                  Instead of live site, get content from help.keyman.com.local"
    exit 1
}

KMA_ROOT="$KEYMAN_ROOT/android"
HELP_ROOT="$KMA_ROOT/KMAPro/kMAPro/src/main/assets/info"

# Default is using live site content
DO_LOCAL=false

# Parse args
while [[ $# -gt 0 ]] ; do
    key="$1"
    case $key in
        -local)
            DO_LOCAL=true
            ;;
        -h|-?)
            display_usage
            ;;
    esac
    shift # past argument
done

echo
echo "DO_LOCAL: $DO_LOCAL"
echo

LOCAL_HOST="help.keyman.com.local"
PRODUCTION_HOST="help.keyman.com"
STAGING_HOST="help.keyman-staging.com"

if [ "$DO_LOCAL" = true ]; then
  WAIT=
else
  # When running against live servers, it's best to have a small wait between loads.
  # Cloudflare may frown on not waiting, as `wget` is basically a scraper/bot.
  WAIT="--wait=2"
fi

# Allows tracking slightly different settings for different offline-mirroring ops
# should we decide to extract the actual mirroring function into its own file.
#
# iOS uses "--no-directories" instead, at least for now.
OPTIONS="--no-host-directories"

DEVICE="android"
HELP_SITE_SECTION="products/android"

# Clear previous help file downloads (if they exist)
if [ -d "$HELP_ROOT" ]; then
  rm -r "$HELP_ROOT"
fi

# Determine host by tier and local
if [ "$DO_LOCAL" = true ]; then
  HOST="$LOCAL_HOST"
elif [ "$TIER" = "alpha" ] || [ "$TIER" = "beta" ]; then
  HOST="$STAGING_HOST"
else
  HOST="$PRODUCTION_HOST"
fi


do_offline_mirror() {
  # $1 - the form-factor specification.

  # Create local mirror of the help page subdirectory.
  # We don't need /font/deploy folder resources, so they're excluded here.
  # One of the .css files auto-includes them otherwise.
  wget --mirror \
       --convert-links \
       $WAIT \
       --keep-session-cookies \
       --page-requisites \
       --no-parent \
       $OPTIONS \
       --restrict-file-names=windows \
       --exclude-directories /font/deploy \
       --directory-prefix="$HELP_ROOT" \
       --adjust-extension \
       "$HOST/$HELP_SITE_SECTION/$VERSION_RELEASE/index.php?embed=$DEVICE&formfactor=$1"

  # Results in a flat-structured mirror of the specified folder,
  # together with all needed resources within the 'site' folder.
}

do_offline_mirror "phone"

# Now for some magic... we rename the page's folder and redownload with the other setting.
mv "$HELP_ROOT/$HELP_SITE_SECTION/$VERSION_RELEASE" "$HELP_ROOT/$HELP_SITE_SECTION/phone"

do_offline_mirror "tablet"

mv "$HELP_ROOT/$HELP_SITE_SECTION/$VERSION_RELEASE" "$HELP_ROOT/$HELP_SITE_SECTION/tablet"
