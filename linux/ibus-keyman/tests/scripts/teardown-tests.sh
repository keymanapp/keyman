#!/usr/bin/env bash
set -eu

. "$(dirname "$0")/test-helper.inc.sh"

exit_on_package_build

CLEANUP_FILE="$1"
DISPLAY_SERVER="$2"

cleanup "${CLEANUP_FILE}" "${DISPLAY_SERVER}"
