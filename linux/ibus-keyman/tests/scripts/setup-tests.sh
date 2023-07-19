#!/usr/bin/env bash
set -eu

. "$(dirname "$0")/test-helper.inc.sh"

if [ -v KEYMAN_PKG_BUILD ]; then
  # Skip setup during package builds - can't run headless and we won't
  # run the other tests anyway
  exit 0
fi

setup "$1" "$2" "$3"
