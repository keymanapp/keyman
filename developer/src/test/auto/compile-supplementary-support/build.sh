#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "test if KS=1 is in the right files, both in debug and non-debug versions" clean configure build test
builder_parse "$@"

KMN=(i3317_nosupp.kmn i3317_withsupp.kmn i3317_withsupp_incontext.kmn i3317_withsupp_inmatch.kmn i3317_withsupp_innomatch.kmn i3317_withsupp_instore.kmn)
KMC="$KEYMAN_ROOT/developer/src/kmc"

function do_test() {
  # Non-debug keyboard builds
	node "$KMC" build "${KMN[@]}"
  grep -vL 'KS=1' i3317_nosupp.js  # non-match
	grep -l 'KS=1' i3317_withsupp.js # match
	grep -l 'KS=1' i3317_withsupp_incontext.js
	grep -l 'KS=1' i3317_withsupp_inmatch.js
	grep -l 'KS=1' i3317_withsupp_innomatch.js
	grep -l 'KS=1' i3317_withsupp_instore.js

  # Debug keyboard builds
	node "$KMC" build -d "${KMN[@]}"
  grep -vL 'KS=1' i3317_nosupp.js  # non-match
	grep -l 'KS=1' i3317_withsupp.js # match
	grep -l 'KS=1' i3317_withsupp_incontext.js
	grep -l 'KS=1' i3317_withsupp_inmatch.js
	grep -l 'KS=1' i3317_withsupp_innomatch.js
	grep -l 'KS=1' i3317_withsupp_instore.js
}

builder_run_action clean:project        rm -f *.js
builder_run_action test:project         do_test

builder_echo warning "TODO: move to kmc-kmn/tests"
