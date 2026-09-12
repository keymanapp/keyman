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
  "Get latest language-subtag-registry" download+

builder_parse "$@"

do_download() {
  util_curl_download_file_with_retry "https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry" ./language-subtag-registry

  local REGISTRY_VERSION

  REGISTRY_VERSION="$(head -n 1 ./language-subtag-registry | cut -d' ' -f 2 -)"

  builder_echo "Downloaded language-subtag-registry, release version $REGISTRY_VERSION"

  if [[ "$REGISTRY_VERSION" != "$KEYMAN_VERSION_LANGUAGE_SUBTAG_REGISTRY" ]]; then
    builder_warn "The downloaded file version ($REGISTRY_VERSION) differs from minimum-versions.inc.sh ($KEYMAN_VERSION_LANGUAGE_SUBTAG_REGISTRY)."
    builder_warn "Ensure you update minimum-versions.inc.sh accordingly (and run the minimum-versions build)"
  fi
}

builder_run_action download   do_download
