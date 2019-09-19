#!/bin/bash

# Uses the open-source `wget` utility to create an embedding-friendly offline mirror
# equivalent of the online iOS help.

HELP_ROOT=keyman/Keyman/resources/OfflineHelp.bundle/Contents/Resources

if [ -z $VERSION ]; then
  VERSION="12.0"  # Set manually here because alpha versions usually don't have 
                  # matching help yet.
fi

# Clear previous help file downloads (if they exist)
if [ -d "$HELP_ROOT" ]; then
  rm -r "$HELP_ROOT/*"
fi

# Create local mirror of the help page subdirectory.
# We don't need /font/deploy folder resources, so they're excluded here.
# One of the .css files auto-includes them otherwise.
wget --mirror \
     --convert-links \
     --wait=2 \
     --keep-session-cookies \
     --page-requisites \
     --no-parent \
     --restrict-file-names=windows \
     --exclude-directories /font/deploy \
     --directory-prefix="$HELP_ROOT" \
     --no-directories \
     --default-page=index.php \
     --adjust-extension \
     "help.keyman.com/products/iphone-and-ipad/$VERSION/index.php?embed=ios"

# Results in a flat-structured mirror of the iphone-and-ipad/$VERSION folder,
# together with all needed resources within the 'site' folder.