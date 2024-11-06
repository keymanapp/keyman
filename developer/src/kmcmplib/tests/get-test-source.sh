#!/usr/bin/env bash
#
# Finds all keyboards in the keyboards repo (passed in $1) that are valid source
# keyboards (i.e. <area>/<prefix>/<name>/source/<name>.kmn)
#
# Called from meson.build, so this script does not use build-utils.sh. Do not run this
# script directly.
#
set -eu
find "$1" -name '*.kmn' | \
  grep -E '(release|experimental)/([a-z0-9_]+)/([a-z0-9_]+)/source/\3\.kmn$' | \
  grep -vE 'masaram_gondi|anii|sil_kmhmu|fv_statimcets|fv_nuucaanul'
# #12623: exclude masaram_gondi due to #11806
# #12630: exclude anii, sil_kmhmu as ico references have mismatching case
# #12630: exclude fv_statimcets, fv_nuucaanul as these include U+2002 which is not
#         treated as whitespace on mac arch