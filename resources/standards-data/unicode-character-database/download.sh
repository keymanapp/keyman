#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/minimum-versions.inc.sh"

################################ Main script ################################

builder_describe \
  "Downloads Unicode data files, version $KEYMAN_VERSION_UNICODE (see minimum-versions.inc.sh), to be committed to repo." \
  download+

builder_describe_outputs \
  download     /resources/standards-data/unicode-character-database/UnicodeData.txt

builder_parse "$@"

# Used by Developer
BLOCKS_SRC_HREF="https://www.unicode.org/Public/$KEYMAN_VERSION_UNICODE/ucd/Blocks.txt"
BLOCKS_SRC_LOCAL="./Blocks.txt"

UNICODE_DATA_SRC_HREF="https://www.unicode.org/Public/$KEYMAN_VERSION_UNICODE/ucd/UnicodeData.txt"
UNICODE_DATA_SRC_LOCAL="./UnicodeData.txt"

# Used by common/models/wordbreakers for the default Unicode wordbreaker.
WORDBREAK_PROP_SRC_HREF="https://www.unicode.org/Public/$KEYMAN_VERSION_UNICODE/ucd/auxiliary/WordBreakProperty.txt"
WORDBREAK_PROP_SRC_LOCAL="./WordBreakProperty.txt"

EMOJI_DATA_SRC_HREF="https://www.unicode.org/Public/$KEYMAN_VERSION_UNICODE/ucd/emoji/emoji-data.txt"
EMOJI_DATA_SRC_LOCAL="./emoji-data.txt"

function downloadPropertyFile() {
  local SRC="$1"
  local DEST="$2"

  local RETRY=5       # Curl retries this number of times before giving up
  local RETRY_DELAY=5 # Make curl sleep this amount of time before each retry when a transfer has failed

  echo "Downloading ${SRC} - ${RETRY} attempts"
  # local URL_DOWNLOAD_FILE=`curl --retry "$RETRY" --retry-delay "$RETRY_DELAY" --silent "${SRC}" | "$JQ" -r .txt`
  curl --fail --retry "$RETRY" --retry-delay "$RETRY_DELAY" --silent "$SRC" --output "$DEST" || {
      builder_die "Downloading $SRC failed with error $?"
  }
}

do_download() {
  downloadPropertyFile "${BLOCKS_SRC_HREF}"          "${BLOCKS_SRC_LOCAL}"
  downloadPropertyFile "${UNICODE_DATA_SRC_HREF}"    "${UNICODE_DATA_SRC_LOCAL}"

  downloadPropertyFile "${WORDBREAK_PROP_SRC_HREF}"  "${WORDBREAK_PROP_SRC_LOCAL}"
  downloadPropertyFile "${EMOJI_DATA_SRC_HREF}"      "${EMOJI_DATA_SRC_LOCAL}"
}

builder_run_action download  do_download