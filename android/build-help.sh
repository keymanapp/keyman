#!/bin/bash

# Uses the open-source `wget` utility to create an embedding-friendly offline mirror
# equivalent of online help.

# Set to .local to run against a locally-hosted version.
# Leave empty for live runs.
LOCAL=.local
WAIT=

# When running against live servers, it's best to have a small wait between loads.
#WAIT="--wait=2"

DEVICE=android
HELP_SITE_SECTION=products/android
HELP_ROOT=KMAPro/kMAPro/src/main/assets/info

VERSION=`cat ../resources/VERSION.md`

# If TIER is set to "alpha", use the previous version.  Help is usually only updated during
# the "beta" process.
if [ -z $TIER ]; then
  # Prevents an error message on the next check.  Also, manual runs will likely be during beta.
  TIER=beta
fi

if [ $TIER = "alpha" ]; then
  # More readable:  (major).minor
  [[ "$VERSION" =~ ([0-9]+)\.[0-9]+ ]]
  # Construct a decremented version string with .minor set to .0
  VERSION="$((${BASH_REMATCH[1]}-1)).0"
fi

# Clear previous help file downloads (if they exist)
if [ -d "$HELP_ROOT" ]; then
  rm -r "$HELP_ROOT"
fi

# Create local mirror of the help page subdirectory.
# We don't need /font/deploy folder resources, so they're excluded here.
# One of the .css files auto-includes them otherwise.
wget --mirror \
     --convert-links \
     $WAIT \
     --keep-session-cookies \
     --page-requisites \
     --no-parent \
     --no-host-directories \
     --restrict-file-names=windows \
     --exclude-directories /font/deploy \
     --directory-prefix="$HELP_ROOT" \
     --default-page=index.php \
     --adjust-extension \
     "help.keyman.com$LOCAL/$HELP_SITE_SECTION/$VERSION/index.php?embed=$DEVICE"

# Results in a flat-structured mirror of the iphone-and-ipad/$VERSION folder,
# together with all needed resources within the 'site' folder.

# Now for some magic... we rename the -page- folder and redownload with the other setting.

mv ./$HELP_ROOT/$HELP_SITE_SECTION/$VERSION ./$HELP_ROOT/$HELP_SITE_SECTION/phone

# Create local mirror of the help page subdirectory.
# We don't need /font/deploy folder resources, so they're excluded here.
# One of the .css files auto-includes them otherwise.
wget --mirror \
     --convert-links \
     $WAIT \
     --keep-session-cookies \
     --page-requisites \
     --no-parent \
     --no-host-directories \
     --restrict-file-names=windows \
     --exclude-directories /font/deploy \
     --directory-prefix="$HELP_ROOT" \
     --default-page=index.php \
     --adjust-extension \
     "help.keyman.com$LOCAL/$HELP_SITE_SECTION/$VERSION/index.php?embed=$DEVICE"

# Results in a flat-structured mirror of the iphone-and-ipad/$VERSION folder,
# together with all needed resources within the 'site' folder.

# Now for some magic... we rename the -page- folder and redownload with the other setting.

mv ./$HELP_ROOT/$HELP_SITE_SECTION/$VERSION ./$HELP_ROOT/$HELP_SITE_SECTION/tablet