#!/usr/bin/env bash
#
# Finds all keyboards in the keyboards repo (passed in $1) that are valid source
# keyboards (i.e. <area>/<prefix>/<name>/source/<name>.kmn)
#
# Called from meson.build, so this script does not use build-utils.sh. Do not run this
# script directly.
#
set -eu
find "$1" -name '*.kmn' | grep -E '(release|experimental)/([a-z0-9_]+)/([a-z0-9_]+)/source/\3\.kmn$'
