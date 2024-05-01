#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# Imports common Web build-script definitions & functions
. "$KEYMAN_ROOT/web/common.inc.sh"

# ################################ Main script ################################

builder_describe "Builds the Keyman Engine for Web's sample page setups." \
  "@/web/src/app/browser build" \
  "@/web/src/app/ui build" \
  "configure           Does nothing for this project" \
  "clean" \
  "build" \
  "test                Does nothing for this project" \
  "--ci                Does nothing for this project"

# Possible TODO?s
# "upload-symbols   Uploads build product to Sentry for error report symbolification.  Only defined for $DOC_BUILD_EMBED_WEB" \

builder_parse "$@"

builder_describe_outputs \
  build       /web/src/samples/simplest/keymanweb.js

#### Common paths ####

KMW_JS="$KEYMAN_ROOT/web/build/app/browser/release/keymanweb.js"
KMW_MAP="$KEYMAN_ROOT/web/build/app/browser/release/keymanweb.js.map"
UI_ROOT="$KEYMAN_ROOT/web/build/app/ui/release"
RESOURCES="$KEYMAN_ROOT/web/build/app/resources/."

SAMPLE_BASE="$KEYMAN_ROOT/web/src/samples"

#### Build action definitions ####

do_clean() {
  # Sample 1
  rm -f "$SAMPLE_BASE/simplest/keymanweb.js"
  rm -f "$SAMPLE_BASE/simplest/keymanweb.js.map"
  rm -f "$SAMPLE_BASE/simplest/kmwuitoggle.js"
  rm -f "$SAMPLE_BASE/simplest/kmwuitoggle.js.map"
  rm -rf "$SAMPLE_BASE/simplest/osk"
  rm -rf "$SAMPLE_BASE/simplest/ui"

  # Sample 2
  rm -rf "$SAMPLE_BASE/subfolder_toggle/keyman"

  # Sample 3
  rm -rf "$SAMPLE_BASE/subfolder_toolbar/keyman"

  # Sample 4
  rm -rf "$SAMPLE_BASE/complex/root/js"
  rm -rf "$SAMPLE_BASE/complex/root/resources"
}

builder_run_action clean do_clean

#
# Copies all relevant scripts to the specified folder for use with a sample page.
#
# ### Usage
#
# ```bash
#   copy_scripts "$SAMPLE_BASE/sample1/keyman_root" "kmwuitoggle.js"
# ```
#
# ### Parameters
#
# * 1: `dest`              the destination folder to receive script copies
# * 2: `uiModule`          name of the UI module file to copy over (optional)
#
# ### Example
#
copy_scripts() {
  UI=
  if [[ $# -ge 2 ]]; then
    UI=$2
    echo $UI
  fi
  DEST=$1

  mkdir -p "$DEST"
  cp "$KMW_JS"  "$DEST"
  cp "$KMW_MAP" "$DEST"

  if [[ $# -ge 2 ]]; then
    cp "$UI_ROOT/$UI"     "$DEST"
    cp "$UI_ROOT/$UI.map" "$DEST"
  fi
}

copy_resources() {
  DEST=$1
  mkdir -p "$DEST"

  cp -R "$RESOURCES" "$DEST"
}

do_copy() {
  # "simplest" - all relevant files are compatible with default init, same folder as page
  copy_scripts   "$SAMPLE_BASE/simplest/" "kmwuitoggle.js"
  copy_resources "$SAMPLE_BASE/simplest/"

  # "subfolder_toggle" - all relevant files are compatible with default init, in direct subfolder
  copy_scripts   "$SAMPLE_BASE/subfolder_toggle/keyman/" "kmwuitoggle.js"
  copy_resources "$SAMPLE_BASE/subfolder_toggle/keyman/"

  # "subfolder_toolbar" - all relevant files are compatible with default init, in direct subfolder
  copy_scripts   "$SAMPLE_BASE/subfolder_toolbar/keyman/" "kmwuitoolbar.js"
  copy_resources "$SAMPLE_BASE/subfolder_toolbar/keyman/"

  # "complex"
  copy_scripts   "$SAMPLE_BASE/complex/root/js/" "kmwuitoggle.js"
  copy_resources "$SAMPLE_BASE/complex/root/resources/"
}

builder_run_action build do_copy