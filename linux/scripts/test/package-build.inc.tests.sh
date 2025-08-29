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
  #         /build.sh
  #         /README.md
  #         /path1
  #               /build.sh
  #               /README.md
  #         /path2
  #               /build.sh
  #         /path3
  #               /build.sh
  # /subdir2
  #         /build.sh
  #         /path1
  # /subdir3
  #         /build.sh
  #         /path1
  #         /path2
  #         /path3
  # /subdir4
  #         /build.sh
  #         /path1
  #               /build.sh
  #         /path2
  #               /subpath1
  #                        /build.sh
  #               /subpath2
  #                        /build.sh
  #         /path3
  #               /build.sh
  # subdir1/path2, subdir2, subdir4/path1, subdir4/path2/subpath1,
  # and subdir4/path3 will be ignored
  mkdir -p "${temp_dir}/subdir1/path1"          # include
  touch "${temp_dir}/subdir1/build.sh"
  touch "${temp_dir}/subdir1/README.md"         # exclude
  touch "${temp_dir}/subdir1/path1/build.sh"    # include
  touch "${temp_dir}/subdir1/path1/README.md"   # exclude
  mkdir -p "${temp_dir}/subdir1/path2"          # exclude
  touch "${temp_dir}/subdir1/path2/build.sh"
  mkdir -p "${temp_dir}/subdir1/path3"          # include
  touch "${temp_dir}/subdir1/path3/build.sh"
  mkdir -p "${temp_dir}/subdir2/path1"          # exclude
  touch "${temp_dir}/subdir2/build.sh"
  mkdir -p "${temp_dir}/subdir3/path1"          # include
  touch "${temp_dir}/subdir3/build.sh"
  mkdir -p "${temp_dir}/subdir3/path2"          # include
  mkdir -p "${temp_dir}/subdir3/path3"          # include
  mkdir -p "${temp_dir}/subdir4/path1"          # exclude
  touch "${temp_dir}/subdir4/build.sh"
  touch "${temp_dir}/subdir4/path1/build.sh"
  mkdir -p "${temp_dir}/subdir4/path2/subpath1" # exclude
  touch "${temp_dir}/subdir4/path2/subpath1/build.sh"
  mkdir -p "${temp_dir}/subdir4/path2/subpath2" # include
  touch "${temp_dir}/subdir4/path2/subpath2/build.sh"
  mkdir -p "${temp_dir}/subdir4/path3"          # exclude
  touch "${temp_dir}/subdir4/path3/build.sh"
  touch "${temp_dir}/subdir4/path3/other.txt"
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
    --tar-ignore=test1/subdir1/path2
    --tar-ignore=test1/subdir1/README.md
    --tar-ignore=test1/subdir2
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
    --tar-ignore=keyman/subdir1/path1/build.sh  # in exclude
    --tar-ignore=keyman/subdir1/path2           # not in include
    --tar-ignore=keyman/subdir1/path3/build.sh  # in exclude
    --tar-ignore=keyman/subdir1/build.sh        # not in include
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
    --tar-ignore=xyz/subdir1/path2            # not in include
    --tar-ignore=xyz/subdir1/README.md        # in exclude
    --tar-ignore=xyz/subdir2                  # not in include
    --tar-ignore=xyz/subdir4/path1            # not in include
    --tar-ignore=xyz/subdir4/path2            # not in include
    --tar-ignore=xyz/subdir4/path3/other.txt  # not in include
    --tar-ignore=xyz/subdir4/path2/subpath1   # not necessary, but in exclude
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
    --tar-ignore=bar/subdir1/path2           # not in include
    --tar-ignore=bar/subdir1/README.md       # not in include
    --tar-ignore=bar/subdir2                 # not in include
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
    --tar-ignore=baz/subdir1
    --tar-ignore=baz/subdir2
    --tar-ignore=baz/subdir3
    --tar-ignore=baz/subdir4/path2/subpath2
  )

  assert-equal "${ignored_files[*]}" "${expected[*]}"
}


# shellcheck disable=2119
run_tests
