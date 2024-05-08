#!/usr/bin/env bash

set -e
set -u
set -x

#
# Usage: min-ver.sh
#
# Parses minimum-versions.inc.sh to update template file minimum-versions.md.in.
# If minimum-versions.md is changed, commit and push changes
#

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "${THIS_SCRIPT%/*}/minimum-versions.inc.sh"

function generate_table {
  echo ""
  echo "## minimum-versions.inc.sh Variables"
  echo ""
  echo "|          KEYMAN Variable          |     Value    |"
  echo "|-----------------------------------|--------------|"

  # Parameter expansion all the KEYMAN_MIN_* variable names into strarr array
  readarray -d ' ' -t strarr <<< ${!KEYMAN_MIN_*}
  for n in ${!strarr[@]}; do
    echo "| ${strarr[n]} | ${!strarr[n]} |"
  done

  # Parameter expansion all the KEYMAN_MAX_* variable names into strarr array
  readarray -d ' ' -t strarr <<< ${!KEYMAN_MAX*}
  for n in ${#strarr[@]}; do
    echo "| ${strarr[n]} | ${!strarr[n]} |"
  done

  # Parameter expansion all the KEYMAN_DEFAULT_* variable names into strarr array
  readarray -d ' ' -t strarr <<< ${!KEYMAN_DEFAULT*}
  for n in ${#strarr[@]}; do
    echo "| ${strarr[n]} | ${!strarr[n]} |"
  done

}

generate_table > minimum-versions.md
