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
  "Get latest ISO639-3 tables" download+

builder_parse "$@"

do_download() {
  util_curl_download_file_with_retry "https://iso639-3.sil.org/sites/iso639-3/files/downloads/iso-639-3.tab" iso639-3.tab

  local DOWNLOADED_DATE

  DOWNLOADED_DATE="$(date '+%Y-%m-%d')"

  builder_echo "Downloaded iso639-3.tab versioned as 'today' ($DOWNLOADED_DATE)"

  if [[ "$DOWNLOADED_DATE" != "$KEYMAN_VERSION_ISO639_3" ]]; then
    builder_warn "The downloaded file date ($DOWNLOADED_DATE) differs from minimum-versions.inc.sh ($KEYMAN_VERSION_ISO639_3)."
    builder_warn "Ensure you update minimum-versions.inc.sh accordingly (and run the minimum-versions build)"
  fi
}

builder_run_action download   do_download
