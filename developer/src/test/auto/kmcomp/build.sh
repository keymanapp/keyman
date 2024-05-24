#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Test that kmc passes various compile tests" clean configure build test
builder_parse "$@"

KMN=(i3317_nosupp.kmn i3317_withsupp.kmn i3317_withsupp_incontext.kmn i3317_withsupp_inmatch.kmn i3317_withsupp_innomatch.kmn i3317_withsupp_instore.kmn)
KMC="$KEYMAN_ROOT/developer/src/kmc"

function should-pass() {
	local title="$1"
	local kmn="$2"
	builder_echo blue "TEST: $title"
	if node "$KMC" build --no-color --log-level hint --compiler-warnings-as-errors "$kmn"; then
		builder_echo green "TEST PASSED"
		return 0
	else
		builder_echo red   "FAILED: expected $kmn to be valid"
		return 1
	fi
}

function should-fail() {
	local title="$1"
	local kmn="$2"
	builder_echo blue "TEST: $title"
	if node "$KMC" build --no-color --log-level hint --compiler-warnings-as-errors "$kmn"; then
		builder_echo red   "FAILED: expected $kmn to be invalid"
		return 1
	else
		builder_echo green "TEST PASSED"
		return 0
	fi
}

function do_test() {
  # Non-debug keyboard builds
	# test-1
	should-pass "Valid .kmn, to .js and .kmx, no warnings" test_valid.kmn || exit 1
	should-pass ".kps to .kmp, no warnings" test_valid.kps || exit 2

	should-fail ".kmn to .kmx, with warning" test_invalid_kmx.kmn || exit 3
	should-fail ".kmn to .js, with warning" test_invalid_js.kmn || exit 4
	should-fail ".kps to .kmp, with warning" test_invalid.kps || exit 5

	should-fail "10.0 .kmn with 14.0-only touch layout codes" test_touchlayout_14_1.kmn || exit 6
	should-pass "14.0 .kmn with 14.0 touch layout codes" test_touchlayout_14_2.kmn || exit 7
	should-pass "9.0 .kmn with accepted 14.0 touch layout codes" test_touchlayout_14_2.kmn || exit 8

	# test-4280
	should-fail "#4280: if should be at start of context 1" test_4280_if_start_1.kmn || exit 9
	should-fail "#4280: if should be at start of context 2" test_4280_if_start_2.kmn || exit 10
	should-fail "#4280: nul should be at start of context 1" test_4280_nul_start_1.kmn || exit 11
	should-fail "#4280: nul should be at start of context 2" test_4280_nul_start_2.kmn || exit 12

	#test-4423
	should-pass "#4423: named code constants, various tests" test_namedcodeconstants.kmn || exit 13

	#test-2241
	should-pass "#2241: expansions" test_expansion.kmn || exit 14
	should-pass "#2241: expansions" test_expansion_1.kmn || exit 15
	# the two files should be identical.
	diff -q test_expansion.kmx test_expansion_1.kmx || exit 16

	should-fail "#2241: invalid expansions" test_expansion_invalid.kmn || exit 17
	should-fail "#2241: expansion absurdly long" test_expansion_absurd.kmn || exit 18

	should-pass "#2241: &CasedKeys" test_casedkeys.kmn || exit 19
	should-pass "#2241: &CasedKeys (chars)" test_casedkeys_chars.kmn || exit 20

	should-fail "#2241: &CasedKeys (mnemonic 1)" test_casedkeys_mnemonic_1.kmn || exit 21
	should-fail "#2241: &CasedKeys (mnemonic 2)" test_casedkeys_mnemonic_2.kmn || exit 22
	should-fail "#2241: &CasedKeys (mnemonic 3)" test_casedkeys_mnemonic_3.kmn || exit 23
	should-fail "#2241: &CasedKeys (invalid chars 1)" test_casedkeys_invalid_1.kmn || exit 24
	should-fail "#2241: &CasedKeys (invalid chars 2)" test_casedkeys_invalid_2.kmn || exit 25

	#test-5963
	should-pass "#5963 start-of-sentence" test_5963_start_of_sentence.kmn || exit 26
	should-fail "#5963 begin newcontext (missing group)" test_5963_newcontext_1.kmn || exit 27
	should-fail "#5963 begin newcontext (not readonly)" test_5963_newcontext_2.kmn || exit 28
	should-fail "#5963 begin postkeystroke (missing group)" test_5963_postkeystroke_1.kmn || exit 29
	should-fail "#5963 begin postkeystroke (not readonly) " test_5963_postkeystroke_2.kmn || exit 30
	should-fail "#5963 context not first token in readonly group output" test_5963_readonlygroup_misplacedcontext.kmn || exit 31
	should-fail "#5963 emitting chars in readonly group" test_5963_readonlygroup_output.kmn || exit 32
	should-fail "#5963 using non-readonly group in readonly group" test_5963_readonlygroup_usenonreadonly.kmn || exit 33

	#test-6440
	# CHINT_UnreachableRule from compiler.rc
	CHINT_UnreachableRule="This rule will never be matched as another rule takes precedence"
	# should-have-message "#6440 hint on unreachable code #1" test_6440_unreachable_code_1.kmn "$CHINT_UnreachableRule" || exit 34
	# should-have-message "#6440 hint on unreachable code #2" test_6440_unreachable_code_2.kmn "$CHINT_UnreachableRule" || exit 35
	# should-have-message "#6440 hint on unreachable code #3" test_6440_unreachable_code_3.kmn "$CHINT_UnreachableRule" || exit 36
	# should-have-message "#6440 hint on unreachable code #4" test_6440_unreachable_code_4.kmn "$CHINT_UnreachableRule" || exit 37
	should-pass "#6440 hint on unreachable code #5" test_6440_unreachable_code_5.kmn || exit 38

	#test-194
	# should-have-message -f test_194_filename_case.out.txt "#194 filename case" test_194_filename_case.kmn || exit 34

	should-fail "#6462 duplicate group #1" test_6462_duplicate_group_1.kmn || exit 39
	should-fail "#6462 duplicate group #2" test_6462_duplicate_group_2.kmn || exit 40
	should-fail "#6462 duplicate store #1" test_6462_duplicate_store_1.kmn || exit 41
	should-fail "#6462 duplicate store #2" test_6462_duplicate_store_2.kmn || exit 42
	should-fail "#6462 duplicate store #3" test_6462_duplicate_store_3.kmn || exit 42
}

builder_run_action clean:project        rm -f *.js *.kmx *.kmp *.kpj.user
builder_run_action test:project         do_test

builder_echo warning "TODO: move to kmc-kmn/tests"
