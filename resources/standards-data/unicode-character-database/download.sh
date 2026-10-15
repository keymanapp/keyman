#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/minimum-versions.inc.sh"
. "$KEYMAN_ROOT/resources/build/utils.inc.sh"

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

# Used by web/src/engine/predictive-text/wordbreakers for the default Unicode wordbreaker.
WORDBREAK_PROP_SRC_HREF="https://www.unicode.org/Public/$KEYMAN_VERSION_UNICODE/ucd/auxiliary/WordBreakProperty.txt"
WORDBREAK_PROP_SRC_LOCAL="./WordBreakProperty.txt"

EMOJI_DATA_SRC_HREF="https://www.unicode.org/Public/$KEYMAN_VERSION_UNICODE/ucd/emoji/emoji-data.txt"
EMOJI_DATA_SRC_LOCAL="./emoji-data.txt"

do_download() {
  util_curl_download_file_with_retry "${BLOCKS_SRC_HREF}"          "${BLOCKS_SRC_LOCAL}"
  util_curl_download_file_with_retry "${UNICODE_DATA_SRC_HREF}"    "${UNICODE_DATA_SRC_LOCAL}"

  util_curl_download_file_with_retry "${WORDBREAK_PROP_SRC_HREF}"  "${WORDBREAK_PROP_SRC_LOCAL}"
  util_curl_download_file_with_retry "${EMOJI_DATA_SRC_HREF}"      "${EMOJI_DATA_SRC_LOCAL}"
}

builder_run_action download  do_download
