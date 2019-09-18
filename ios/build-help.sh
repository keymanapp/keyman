#!/bin/bash

# Uses the open-source `wget` utility to create an embedding-friendly offline mirror
# equivalent of the online iOS help.

$HELP_ROOT=keyman/Keyman/Keyman/WebPages

if [ -z $VERSION ]; then
  $VERSION="11.0"
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
     --page-requisites \
     --no-parent \
     --restrict-file-names=windows \
     --exclude-directories /font/deploy \
     --directory-prefix="$HELP_ROOT" \
     --no-directories \
     --default-page=index.php \
     --adjust-extension \
     "help.keyman.com/products/iphone-and-ipad/$VERSION/index.php"

# Results in a flat-structured mirror of the iphone-and-ipad/11.0 folder,
# together with all needed resources within the 'site' folder.