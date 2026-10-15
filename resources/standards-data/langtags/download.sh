#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

source "$KEYMAN_ROOT/resources/build/jq.inc.sh"
source "$KEYMAN_ROOT/resources/build/utils.inc.sh"
source "$KEYMAN_ROOT/resources/build/minimum-versions.inc.sh"

builder_describe \
  "Get latest langtags.json" \
  "download+   Download latest release langtags.json" \
  "staging     Download pre-release staging langtags.json"

builder_parse "$@"

do_download() {
  local name="$1"
  local url="$2"

  util_curl_download_file_with_retry "$url" ./langtags.json

  local LANGTAGS_API_VERSION LANGTAGS_DATE

  LANGTAGS_API_VERSION="$("$JQ" -r '.[] | select( .tag == "_version" ) | .api' < langtags.json)"
  LANGTAGS_DATE="$("$JQ" -r '.[] | select( .tag == "_version" ) | .date' < langtags.json)"

  builder_echo "Downloaded $name langtags.json, API version $LANGTAGS_API_VERSION, released $LANGTAGS_DATE"

  if [[ "$LANGTAGS_API_VERSION" != "$KEYMAN_VERSION_LANGTAGS_API" ]]; then
    builder_die "The downloaded $name langtags.json has version '$LANGTAGS_API_VERSION', which does not match expected '$KEYMAN_VERSION_LANGTAGS_API'"
  fi

  if [[ "$LANGTAGS_DATE" != "$KEYMAN_VERSION_LANGTAGS" ]]; then
    builder_warn "The downloaded $name langtags.json date ($LANGTAGS_DATE) differs from minimum-versions.inc.sh ($KEYMAN_VERSION_LANGTAGS)."
    builder_warn "Ensure you update minimum-versions.inc.sh accordingly (and run the minimum-versions build)"
  fi
}

builder_run_action download   do_download release 'https://ldml.api.sil.org/langtags.json'
builder_run_action staging    do_download staging 'https://ldml.api.sil.org/langtags.json?staging=1'

