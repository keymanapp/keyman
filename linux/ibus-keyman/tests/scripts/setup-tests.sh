#!/usr/bin/env bash
set -eu

. "$(dirname "$0")/test-helper.inc.sh"

exit_on_package_build

DISPLAY_SERVER="$1"
ENV_FILE="$2"
CLEANUP_FILE="$3"
PID_FILE="$4"

setup "${DISPLAY_SERVER}" "${ENV_FILE}" "${CLEANUP_FILE}" "${PID_FILE}"
