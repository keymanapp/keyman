#
# Keyman is copyright (C) SIL Global. MIT License.
#
# Created by mcdurdin on 2025-04-30
#
# Test that resources in our repository match the expected minimum versions,
# where possible. Uses a variety of mechanisms to extract the version
#

. "${KEYMAN_ROOT}/resources/build/jq.inc.sh"

#
# Tests that the referenced filename has the expected version
# found in the input variable. The actual version is extracted
# with a helper function named _minver_test_$variable, which
# parses the file or its metadata, as this may vary considerably
#
# Parameters
#   1: variable    the name of the environment variable to test
#   2: filename    the file to test the version against
#
_minver_test() {
  local variable="$1"
  local filename="$2"

  local actual_version="$(_minver_test_${variable} "$filename")"
  local -n expected_version="${variable}"

  builder_echo "Testing \$$1, expecting '$expected_version'"
  if [[ "$expected_version" != "$actual_version" ]]; then
    builder_die "$filename: expected '$expected_version', actual version in repo is '$actual_version'"
  fi
}

_minver_test_KEYMAN_VERSION_LANGUAGE_SUBTAG_REGISTRY() {
  # language-subtag-registry format:
  #   File-Date: 2025-03-10
  head "$1" -n 1 | cut -d" " -f 2 -
}

_minver_test_KEYMAN_VERSION_UNICODE() {
  # Blocks.txt format:
  #   # Blocks-16.0.0.txt
  head "$1" -n 1 | cut -d- -f 2 - | cut -d. -f 1,2,3 -
}

_minver_test_KEYMAN_VERSION_LANGTAGS() {
  # entry with tag '_version'
  "$JQ" -r -c '.[] | select(.tag == "_version") .date' "$1"
}

_minver_test_KEYMAN_VERSION_CLDR() {
  # finds the folder with the expected name
  ## find "$1" -name '[0-9][0-9]' -type d -printf '%f'
  # note macos find does not support -printf, so went with this:
  find "$1" -name '[0-9][0-9]' -type d -exec basename {} \;
}

_minver_test_KEYMAN_VERSION_ISO639_3() {
  # git commit log for last commit for the file
  git log -1 --format="%cs" -- "$1"
}

_minver_test_KEYMAN_VERSION_ICU() {
  # source_filename = icu4c-73_1-src.tgz
  grep "source_filename" "$1" | cut -d- -f 2 - | tr _ .
}

_minver_test_KEYMAN_MIN_VERSION_NODE_MAJOR() {
  # package.json, major.minor.patch, we use only major
  "$JQ" -r '.engines.node' "$1" | cut -d. -f 1 -
}

#
# Test that the minimum version of imported resources matches as expected
#
minver_test_all() {
  local STANDARDS_DATA="$KEYMAN_ROOT/resources/standards-data"

  _minver_test KEYMAN_MIN_VERSION_NODE_MAJOR "$KEYMAN_ROOT/package.json"

  _minver_test KEYMAN_VERSION_ICU "$KEYMAN_ROOT/core/subprojects/icu-minimal.wrap"

  _minver_test KEYMAN_VERSION_LANGUAGE_SUBTAG_REGISTRY "$STANDARDS_DATA/language-subtag-registry/language-subtag-registry"
  _minver_test KEYMAN_VERSION_UNICODE "$STANDARDS_DATA/unicode-character-database/Blocks.txt"
  _minver_test KEYMAN_VERSION_UNICODE "$STANDARDS_DATA/unicode-character-database/WordBreakProperty.txt"
  _minver_test KEYMAN_VERSION_LANGTAGS "$STANDARDS_DATA/langtags/langtags.json"
  _minver_test KEYMAN_VERSION_CLDR "$STANDARDS_DATA/ldml-keyboards/"
  _minver_test KEYMAN_VERSION_ISO639_3 "$STANDARDS_DATA/iso639-3/iso639-3.tab"
}