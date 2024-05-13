#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../../resources/build/builder.inc.sh"
# END STANDARD BUILD SCRIPT INCLUDE

builder_describe \
  "Tests child/dependency builds not run twice" \
  configure build

builder_parse "$@"

if builder_start_action build; then
  echo "child/dep: do_build"
  echo "$@" >> ./actual-calls
  builder_finish_action success build
fi

