#!/usr/bin/env bash
set -eu

. "$(dirname "$0")/../../tests/scripts/test-helper.inc.sh"

setup_display_server_only "$1" "$2" "$3"
