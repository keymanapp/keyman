#!/usr/bin/env bash
set -eu

. "$(dirname "$0")/test-helper.inc.sh"

exit_on_package_build

cleanup "$1"
