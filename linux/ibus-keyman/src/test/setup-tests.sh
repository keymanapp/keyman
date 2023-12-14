#!/usr/bin/env bash
set -eu

. "$(dirname "$0")/../../tests/scripts/test-helper.inc.sh"

setup_display_server_only "${1:-x11}" "${2:-/tmp/env-src-test.txt}" "${3:-/tmp/ibus-keyman-src-test-pids}" "${4:-/tmp/ibus-keyman-src-test-pids.pids}"
