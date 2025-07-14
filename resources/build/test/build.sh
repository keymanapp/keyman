#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

################################ Main script ################################

builder_describe \
  "Run unit tests (defined in *.test*.sh or test.sh) for builder scripts" \
  "test"

builder_parse "$@"

do_test() {
  set +e

  # TODO: return non-0 exit code if any of the tests fail
  while IFS= read -r -d '' file
  do
    builder_echo start "${file}" "Running tests for ${file}"
    "${file}"
    builder_echo end "${file}" success "Finished running tests for ${file}"
  done < <(find . -name \*.test*.sh -o -name test.sh -print0)
}

builder_run_action test  do_test
