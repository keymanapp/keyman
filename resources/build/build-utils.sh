#!/usr/bin/env bash

# TODO: remove this file in v19.0. Used only by deb-packaging.yml
echo "WARNING: build-utils.sh is deprecated. Use instead builder-basic.inc.sh"
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/builder-basic.inc.sh"
