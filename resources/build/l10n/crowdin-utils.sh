#!/bin/bash
# Common paths and functions for download-crowdin and upload-crowdin scripts

# Path definitions
KMA_ROOT="$KEYMAN_ROOT/android"
KMI_ROOT="$KEYMAN_ROOT/ios"
CROWDIN_TMP_ROOT="$KEYMAN_ROOT/.crowdin-tmp"
CROWDIN_ROOT="$KEYMAN_ROOT/crowdin"
KEYMAN_ZIP="$CROWDIN_TMP_ROOT/Keyman.zip"


# If a source file $1 exists, copy it to a destination directory $2.
# If destination doesn't exist, create it
# copy_file [source file] [destination directory]
function copy_file() {
  if [ -f "$1" ]; then
    if [ ! -d "$2" ]; then
      echo "Creating $2 directory"
      mkdir -p "$2"
    fi
    echo "Copying $1"
    cp "$1" "$2"
  fi
}


