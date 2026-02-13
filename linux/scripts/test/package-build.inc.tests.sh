#!/usr/bin/env bash
# shellcheck disable=2034,1091,2154
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-basic.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "${KEYMAN_ROOT}/resources/build/test/testing-framework.inc.sh"
. "${KEYMAN_ROOT}/linux/scripts/package-build.inc.sh"

function setup_file() {
  temp_dir="$(mktemp -d)"
  # Create directory structure for testing
  # /
  # /subdir1
  #         /build/foo
  #         /build.sh
  #         /README.md
  #         /path1
  #               /abc.build
  #               /build/foo
  #               /build.sh
  #               /README.md
  #               /x/z.txt
  #               /xy/z.txt
  #         /path2
  #               /build/foo
  #               /build.sh
  #         /path3
  #               /build/foo
  #               /build.sh
  # /subdir2
  #         /build/foo
  #         /build.sh
  #         /path1
  #               /build/foo
  # /subdir3
  #         /build/foo
  #         /build.sh
  #         /path1
  #               /build/foo
  #         /path2
  #               /build/foo
  #         /path3
  # /subdir4
  #         /build/foo
  #         /build.sh
  #         /path1
  #               /build.sh
  #         /path2
  #               /subpath1
  #                        /build/foo
  #                        /build.sh
  #               /subpath2
  #                        /build/foo
  #                        /build.sh
  #         /path3
  #               /build.sh
  # /build.sh
  # subdir1/path2, subdir2, subdir4/path1, subdir4/path2/subpath1,
  # and subdir4/path3 will be ignored
  # all build/ directories will be ignored with the exception of
  # subdir3/build.
  mkdir -p "${temp_dir}/subdir1/build"          # exclude
  touch "${temp_dir}/subdir1/build/foo"         # exclude
  mkdir -p "${temp_dir}/subdir1/path1/build"
  touch "${temp_dir}/subdir1/path1/build/foo"   # exclude
  mkdir -p "${temp_dir}/subdir1/path1/x"
  touch "${temp_dir}/subdir1/path1/x/z.txt"
  mkdir -p "${temp_dir}/subdir1/path1/xy"
  touch "${temp_dir}/subdir1/path1/xy/z.txt"
  touch "${temp_dir}/subdir1/build.sh"          # include
  touch "${temp_dir}/subdir1/README.md"         # exclude
  touch "${temp_dir}/subdir1/path1/abc.build"
  touch "${temp_dir}/subdir1/path1/build.sh"    # include
  touch "${temp_dir}/subdir1/path1/README.md"   # exclude
  mkdir -p "${temp_dir}/subdir1/path2/build"
  touch "${temp_dir}/subdir1/path2/build/foo"   # exclude
  touch "${temp_dir}/subdir1/path2/build.sh"    # exclude
  mkdir -p "${temp_dir}/subdir1/path3/build"
  touch "${temp_dir}/subdir1/path2/build/foo"   # exclude
  touch "${temp_dir}/subdir1/path3/build.sh"    # include
  mkdir -p "${temp_dir}/subdir2/build"          # exclude
  touch "${temp_dir}/subdir2/build/foo"         # exclude
  mkdir -p "${temp_dir}/subdir2/path1/build"    # exclude
  touch "${temp_dir}/subdir2/path1/build/foo"   # exclude
  touch "${temp_dir}/subdir2/build.sh"
  mkdir -p "${temp_dir}/subdir3/build"          # include
  touch "${temp_dir}/subdir3/build/foo"         # include
  mkdir -p "${temp_dir}/subdir3/path1"          # include
  touch "${temp_dir}/subdir3/build.sh"
  mkdir -p "${temp_dir}/subdir3/path1/build"    # exclude
  touch "${temp_dir}/subdir3/path1/build/foo"   # exclude
  mkdir -p "${temp_dir}/subdir3/path2"          # include
  mkdir -p "${temp_dir}/subdir3/path2/build"    # exclude
  touch "${temp_dir}/subdir3/path2/build/foo"   # exclude
  mkdir -p "${temp_dir}/subdir3/path3"          # include
  mkdir -p "${temp_dir}/subdir4/build"          # exclude
  touch "${temp_dir}/subdir4/build/foo"         # exclude
  mkdir -p "${temp_dir}/subdir4/path1"          # exclude
  touch "${temp_dir}/subdir4/build.sh"
  touch "${temp_dir}/subdir4/path1/build.sh"
  mkdir -p "${temp_dir}/subdir4/path2/subpath1" # exclude
  touch "${temp_dir}/subdir4/path2/subpath1/build.sh"
  mkdir -p "${temp_dir}/subdir4/path2/subpath1/build" # exclude
  touch "${temp_dir}/subdir4/path2/subpath1/build/foo"   # exclude
  mkdir -p "${temp_dir}/subdir4/path2/subpath2" # include
  touch "${temp_dir}/subdir4/path2/subpath2/build.sh"
  mkdir -p "${temp_dir}/subdir4/path2/subpath2/build" # exclude
  touch "${temp_dir}/subdir4/path2/subpath2/build/foo"   # exclude
  mkdir -p "${temp_dir}/subdir4/path3"          # exclude
  touch "${temp_dir}/subdir4/path3/build.sh"
  touch "${temp_dir}/subdir4/path3/other.txt"
  touch "${temp_dir}/build.sh"
}

function teardown_file() {
  rm -rf "${temp_dir}"
}

function test__generate_tar_ignore_list__basic() {
  to_include=(subdir1/path1 subdir1/path3 subdir3 subdir4/path2)
  to_exclude=(subdir1/README.md subdir4/path2/subpath1 subdir4/path2/subpath3)

  cd "${temp_dir}"
  ignored_files=()

  # Execute
  generate_tar_ignore_list "./" to_include to_exclude ignored_files test1

  # Verify
  expected=(
    --tar-ignore=test1/build.sh
    --tar-ignore=test1/subdir1/build
    --tar-ignore=test1/subdir1/build.sh
    --tar-ignore=test1/subdir1/path2
    --tar-ignore=test1/subdir1/README.md
    --tar-ignore=test1/subdir2
    --tar-ignore=test1/subdir4/build
    --tar-ignore=test1/subdir4/build.sh
    --tar-ignore=test1/subdir4/path1
    --tar-ignore=test1/subdir4/path2/subpath1
    --tar-ignore=test1/subdir4/path3
  )

  assert-equal "${ignored_files[*]}" "${expected[*]}"
}

function test__generate_tar_ignore_list__path_wildcard_in_exclude() {
  # Setup
  to_include=(subdir1/path1 subdir1/path3 subdir3)
  to_exclude=(subdir1/*/README.md subdir4/path2/subpath1 subdir4/path2/subpath3)

  cd "${temp_dir}"
  ignored_files=()

  # Execute
  generate_tar_ignore_list "./" to_include to_exclude ignored_files foo

  # Verify
  expected=(
    --tar-ignore=foo/build.sh                 # not in include
    --tar-ignore=foo/subdir1/build            # not in include
    --tar-ignore=foo/subdir1/build.sh         # not in include
    --tar-ignore=foo/subdir1/path1/README.md  # in exclude
    --tar-ignore=foo/subdir1/path2            # not in include
    --tar-ignore=foo/subdir1/README.md        # not in include
    --tar-ignore=foo/subdir2                  # not in include
    --tar-ignore=foo/subdir4                  # not in include
  )

  assert-equal "${ignored_files[*]}" "${expected[*]}"
}

function test__generate_tar_ignore_list__file_wildcard_in_exclude() {
  # Setup
  to_include=(subdir1/path1 subdir1/path3 subdir3)
  to_exclude=(*.sh)

  cd "${temp_dir}"
  ignored_files=()

  # Execute
  generate_tar_ignore_list "./" to_include to_exclude ignored_files keyman

  # Verify
  expected=(
    --tar-ignore=keyman/build.sh                # not in include
    --tar-ignore=keyman/subdir1/build           # not in include
    --tar-ignore=keyman/subdir1/build.sh        # not in include
    --tar-ignore=keyman/subdir1/path1/build.sh  # in exclude
    --tar-ignore=keyman/subdir1/path2           # not in include
    --tar-ignore=keyman/subdir1/path3/build.sh  # in exclude
    --tar-ignore=keyman/subdir1/README.md       # not in include
    --tar-ignore=keyman/subdir2                 # not in include
    --tar-ignore=keyman/subdir3/build.sh        # in exclude
    --tar-ignore=keyman/subdir4                 # not in include
  )

  assert-equal "${ignored_files[*]}" "${expected[*]}"
}

function test__generate_tar_ignore_list__file_wildcard_in_include() {
  # Setup
  to_include=(subdir1/path1 subdir1/path3 subdir3 subdir4/path3/*.sh)
  to_exclude=(subdir1/README.md subdir4/path2/subpath1 subdir4/path2/subpath3)

  cd "${temp_dir}"
  ignored_files=()

  # Execute
  generate_tar_ignore_list "./" to_include to_exclude ignored_files xyz

  # Verify
  expected=(
    --tar-ignore=xyz/build.sh                 # not in include
    --tar-ignore=xyz/subdir1/build            # not in include
    --tar-ignore=xyz/subdir1/build.sh         # not in include
    --tar-ignore=xyz/subdir1/path2            # not in include
    --tar-ignore=xyz/subdir1/README.md        # in exclude
    --tar-ignore=xyz/subdir2                  # not in include
    --tar-ignore=xyz/subdir4/build            # not in include
    --tar-ignore=xyz/subdir4/build.sh         # not in include
    --tar-ignore=xyz/subdir4/path1            # not in include
    --tar-ignore=xyz/subdir4/path2            # not in include
    --tar-ignore=xyz/subdir4/path3/other.txt  # not in include
  )

  assert-equal "${ignored_files[*]}" "${expected[*]}"
}

function test__generate_tar_ignore_list__path_wildcard_in_include() {
  # Setup
  to_include=(subdir1/path1 subdir1/path3 subdir3 subdir4/*/subpath2)
  to_exclude=(subdir1/README.md)

  cd "${temp_dir}"
  ignored_files=()

  # Execute
  generate_tar_ignore_list "./" to_include to_exclude ignored_files bar

  # Verify
  expected=(
    --tar-ignore=bar/build.sh                # not in include
    --tar-ignore=bar/subdir1/build           # not in include
    --tar-ignore=bar/subdir1/build.sh        # not in include
    --tar-ignore=bar/subdir1/path2           # not in include
    --tar-ignore=bar/subdir1/README.md       # not in include
    --tar-ignore=bar/subdir2                 # not in include
    --tar-ignore=bar/subdir4/build           # not in include
    --tar-ignore=bar/subdir4/build.sh        # not in include
    --tar-ignore=bar/subdir4/path1           # not in include
    --tar-ignore=bar/subdir4/path2/subpath1  # not in include
    --tar-ignore=bar/subdir4/path3           # not in include
  )

  assert-equal "${ignored_files[*]}" "${expected[*]}"
}

function test__generate_tar_ignore_list__exclude_subsubdir() {
  to_include=(subdir4)
  to_exclude=(subdir4/path2/subpath2)

  cd "${temp_dir}"
  ignored_files=()

  # Execute
  generate_tar_ignore_list "./" to_include to_exclude ignored_files baz

  # Verify
  expected=(
    --tar-ignore=baz/build.sh               # not in include
    --tar-ignore=baz/subdir1                # not in include
    --tar-ignore=baz/subdir2                # not in include
    --tar-ignore=baz/subdir3                # not in include
    --tar-ignore=baz/subdir4/path2/subpath2 # in exclude
  )

  assert-equal "${ignored_files[*]}" "${expected[*]}"
}

function test__generate_tar_ignore_list__exclude_build_Dirs() {
  to_include=(subdir1/path1 subdir1/path3 subdir3 subdir3/build subdir4/path2)
  to_exclude=(subdir1/README.md subdir4/path2/subpath1 subdir4/path2/subpath3 build)

  cd "${temp_dir}"
  ignored_files=()

  # Execute
  generate_tar_ignore_list "./" to_include to_exclude ignored_files wildcard

  # Verify
  expected=(
    --tar-ignore=wildcard/build.sh                       # not in include
    --tar-ignore=wildcard/subdir1/build                  # in exclude
    --tar-ignore=wildcard/subdir1/build.sh               # not in include
    --tar-ignore=wildcard/subdir1/path1/build            # in exclude
    --tar-ignore=wildcard/subdir1/path2                  # not in include
    --tar-ignore=wildcard/subdir1/path3/build            # in exclude
    --tar-ignore=wildcard/subdir1/README.md              # in exclude
    --tar-ignore=wildcard/subdir2                        # not in include
    --tar-ignore=wildcard/subdir3/path1/build            # in exclude
    --tar-ignore=wildcard/subdir3/path2/build            # in exclude
    --tar-ignore=wildcard/subdir4/build                  # in exclude
    --tar-ignore=wildcard/subdir4/build.sh               # not in include
    --tar-ignore=wildcard/subdir4/path1                  # not in include
    --tar-ignore=wildcard/subdir4/path2/subpath1         # in exclude
    --tar-ignore=wildcard/subdir4/path2/subpath2/build   # in exclude
    --tar-ignore=wildcard/subdir4/path3                  # not in include
  )

  assert-equal "${ignored_files[*]}" "${expected[*]}"
}

function test__generate_tar_ignore_list__exclude_xy() {
  to_include=(subdir1 subdir2 subdir3 subdir4)
  to_exclude=(xy)

  cd "${temp_dir}"
  ignored_files=()

  # Execute
  generate_tar_ignore_list "./" to_include to_exclude ignored_files wildcard

  # Verify
  expected=(
    --tar-ignore=wildcard/build.sh           # not in include
    --tar-ignore=wildcard/subdir1/path1/xy   # in exclude
  )

  assert-equal "${ignored_files[*]}" "${expected[*]}"
}

function test__generate_tar_ignore_list__exclude_x_doesnot_exclude_xy() {
  to_include=(subdir1 subdir2 subdir3 subdir4)
  to_exclude=(x)

  cd "${temp_dir}"
  ignored_files=()

  # Execute
  generate_tar_ignore_list "./" to_include to_exclude ignored_files wildcard

  # Verify
  expected=(
    --tar-ignore=wildcard/build.sh          # not in include
    --tar-ignore=wildcard/subdir1/path1/x   # in exclude
  )

  assert-equal "${ignored_files[*]}" "${expected[*]}"
}

function test__generate_tar_ignore_list__can_include_toplevel_files() {
  to_include=(subdir1 subdir3 ./build.sh)
  to_exclude=(*.sh)

  cd "${temp_dir}"
  ignored_files=()

  # Execute
  generate_tar_ignore_list "./" to_include to_exclude ignored_files top

  # Verify
  expected=(
    --tar-ignore=top/subdir1/build.sh        # in exclude
    --tar-ignore=top/subdir1/path1/build.sh  # in exclude
    --tar-ignore=top/subdir1/path2/build.sh  # in exclude
    --tar-ignore=top/subdir1/path3/build.sh  # in exclude
    --tar-ignore=top/subdir2                 # not in include
    --tar-ignore=top/subdir3/build.sh        # in exclude
    --tar-ignore=top/subdir4                 # not in include
  )

  assert-equal "${ignored_files[*]}" "${expected[*]}"
}

# Tests for _starts_with function
function test__starts_with__match_parent() {
  # Setup
  includes=(subdir1/path1 subdir3)

  # Verify
  assert-true _starts_with includes ./subdir1
}

function test__starts_with__exact_match() {
  # Setup
  includes=(subdir1/path1 subdir3)

  # Verify
  assert-true _starts_with includes ./subdir1/path1
}

function test__starts_with__exact_match_localdir() {
  # Setup
  includes=(./subdir1/path1 ./subdir3)

  # Verify
  assert-true _starts_with includes ./subdir1/path1
}

function test__starts_with__no_match() {
  # Setup
  includes=(subdir1/path1 subdir3)

  # Verify
  assert-false _starts_with includes ./subdir2
}

function test__starts_with__normalized_paths() {
  # Setup
  includes=(path1 path2)

  # Verify
  assert-true _starts_with includes ./path1
}

function test__starts_with__subdir() {
  # Setup
  includes=(path1 path2)

  # Verify
  assert-false _starts_with includes ./path1/subdir
}

function test__starts_with__no_match_different_name() {
  # Setup
  includes=(path1 path2)

  # Verify
  assert-false _starts_with includes ./path3
}

# Tests for _ends_with function
function test__ends_with__exact_filename_first() {
  # Setup
  local array=(README.md test.txt)

  # Verify
  assert-true _ends_with array ./subdir/README.md
}

function test__ends_with__exact_filename_second() {
  # Setup
  local array=(README.md test.txt)

  # Verify
  assert-true _ends_with array ./path/to/test.txt
}

function test__ends_with__not_matching_similar_extension() {
  # Setup
  local array=(README.md test.txt)

  # Verify
  assert-false _ends_with array ./README.md.bak
}

function test__ends_with__no_match_other_filename() {
  # Setup
  local array=(README.md test.txt)

  # Verify
  assert-false _ends_with array ./other.txt
}

# Tests for _is_exact_match function
function test__is_exact_match__full_path_first() {
  # Setup
  includes=(./subdir1/path1 subdir3)

  # Verify
  assert-true _is_exact_match includes ./subdir1/path1
}

function test__is_exact_match__full_path_second() {
  # Setup
  includes=(./subdir1/path1 subdir3)

  # Verify
  assert-true _is_exact_match includes ./subdir3
}

function test__is_exact_match__partial_path_no_match() {
  # Setup
  includes=(./subdir1/path1 subdir3)

  # Verify
  assert-false _is_exact_match includes ./subdir1
}

function test__is_exact_match__sibling_path_no_match() {
  # Setup
  includes=(./subdir1/path1 subdir3)

  # Verify
  assert-false _is_exact_match includes ./subdir1/path2
}

function test__is_exact_match__normalized_paths_first() {
  # Setup
  includes=(subdir1 subdir2)

  # Verify
  assert-true _is_exact_match includes ./subdir1
}

function test__is_exact_match__normalized_paths_second() {
  # Setup
  includes=(subdir1 subdir2)

  # Verify
  assert-true _is_exact_match includes ./subdir2
}

function test__is_exact_match__empty_array() {
  # Setup
  includes=()

  # Verify
  assert-false _is_exact_match includes ./anything
}

# Tests for _is_wildcard_match function
function test__is_wildcard_match__file() {
  # Setup
  local array=(sh)

  # Verify
  assert-true _is_wildcard_match array ./config/sh
}

function test__is_wildcard_match__dot_file() {
  # Setup
  local array=(sh)

  # Verify
  assert-false _is_wildcard_match array ./config/.sh
}

function test__is_wildcard_match__dot_files_nested() {
  # Setup
  local array=(.sh)

  # Verify
  assert-true _is_wildcard_match array ./deeply/nested/build/.sh
}

function test__is_wildcard_match__no_dot_prefix() {
  # Setup
  local array=(sh)

  # Verify
  assert-false _is_wildcard_match array ./build.sh
}

function test__is_wildcard_match__toplevel_file() {
  # Setup
  local array=(sh)

  # Verify
  assert-true _is_wildcard_match array ./sh
}

function test__is_wildcard_match__multiple_extensions_first() {
  # Setup
  local array=(tmp log)

  # Verify
  assert-true _is_wildcard_match array ./path/tmp
}

function test__is_wildcard_match__multiple_extensions_second() {
  # Setup
  local array=(tmp log)

  # Verify
  assert-true _is_wildcard_match array ./path/log
}

function test__is_wildcard_match__dot_in_wildcard_first() {
  # Setup
  local array=(.tmp .log)

  # Verify
  assert-true _is_wildcard_match array ./path/.tmp
}

function test__is_wildcard_match__dot_in_wildcard_second() {
  # Setup
  local array=(.tmp .log)

  # Verify
  assert-true _is_wildcard_match array ./path/.log
}

function test__is_wildcard_match__dot_in_wildcard_no_dotfile() {
  # Setup
  local array=(.tmp .log)

  # Verify
  assert-false _is_wildcard_match array ./path/xtmp
}

function test__is_wildcard_match__wildcard_match_dotfile() {
  # Setup
  local array=(\*.sh)

  # Verify
  assert-true _is_wildcard_match array ./path/.sh
}

function test__is_wildcard_match__wildcard_match_file() {
  # Setup
  local array=(\*.sh)

  # Verify
  assert-true _is_wildcard_match array ./path/subdir/build.sh
}

function test__is_wildcard_match__wildcard_no_matches_file() {
  # Setup
  local array=(\*.sh)

  # Verify
  assert-false _is_wildcard_match array ./path/old
}

function test__is_wildcard_match__wildcard_no_matches_path() {
  # Setup
  local array=(\*.sh)

  # Verify
  assert-false _is_wildcard_match array ./path/build.sh/foo
}

# shellcheck disable=2119
run_tests
