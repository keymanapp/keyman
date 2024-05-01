#!/usr/bin/env bash

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/build-utils.inc.sh"
# END STANDARD BUILD SCRIPT INCLUDE

cd "$THIS_SCRIPT_PATH"

# First, cleanup
./basic/app/build.sh clean
./basic/library/build.sh clean
./targets/app/build.sh clean
./targets/library/build.sh clean

echo ------------------------------------------------------------------
echo Finished cleanup
echo ------------------------------------------------------------------

# Now, run basic dependency test

./basic/app/build.sh build

[[ -f ./basic/app/out.configure && -f ./basic/app/out.build ]] || builder_die "FAIL: expected basic/app to have built output files"
[[ -f ./basic/library/out.configure && -f ./basic/library/out.build ]] || builder_die "FAIL: expected basic/library to have built output files"

# Run a build that only builds a given target from the dependency
./targets/app/build.sh build

[[ -f ./targets/app/out.configure && -f ./targets/app/out.build ]] || builder_die "FAIL: expected target/app to have built output files"
[[ -f ./targets/library/out.first.configure && -f ./targets/library/out.first.build ]] || builder_die "FAIL: expected target/library to have built :first output files"
[[ -f ./targets/library/out.second.configure || -f ./targets/library/out.second.build ]] && builder_die "FAIL: expected target/library to have not built :second output files"

# Run a test where a dependency fails with an error
./error/app/build.sh error && \
  builder_die "FAIL: error code 0 but should have failed with exit code 22 from child dep" || (
    result=$?
    if [[ $result != 22 ]]; then
      builder_die "FAIL: exit code $result but should have failed with exit code 22 from child dep"
    fi
  ) || exit $?

echo "PASS: all dependency tests ran as expected"
