#!/usr/bin/env bash
#
# Tests the pull-requests.inc.sh functions. This is not really intended to be
# used as an automated test, but rather only to be run manually after changes to
# pull-requests.inc.sh.
#

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/build-utils.sh"
# END STANDARD BUILD SCRIPT INCLUDE

cd "$THIS_SCRIPT_PATH"

. "$KEYMAN_ROOT/resources/build/ci/pull-requests.inc.sh"

test_repo_base=`mktemp -d`
test_repo="$test_repo_base/ci-test-repo"
test_filename="$(uuidgen).md"

builder_echo "Preparing local repository clone in $test_repo"

pushd "$test_repo_base" >/dev/null
git clone https://github.com/keymanapp/ci-test-repo
popd >/dev/null

builder_echo "Creating test file $test_filename"

echo "This is a test $test_filename" > "$test_repo/$test_filename"

builder_echo blue "Testing ci_add_files"

ci_add_files "$test_repo" .

builder_echo blue "Testing ci_repo_has_cached_changes"

if ! ci_repo_has_cached_changes "$test_repo"; then
  builder_die "Expected to have a file to commit!"
fi

builder_echo blue "Testing ci_open_pull_request"

ci_open_pull_request "$test_repo" "test/ci" "chore: testing CI"

rm -rf "$test_repo_base"

builder_echo green "Tests passed"
