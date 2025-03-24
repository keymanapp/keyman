#!/usr/bin/env bash
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "${THIS_SCRIPT%/*}/test.inc.sh"

test_check_updated_version_number__NoChange_OK() {
  createBase alpha

  echo "readme" > README.md
  git add README.md
  git commit -m "Some change on the branch"

  echo "## Calling API verification"
  pwd
  linux/scripts/deb-packaging.sh --bin-pkg "${BINPKG_NAME}" --git-sha "$(git rev-parse HEAD)" --git-base master verify
}

test_check_updated_version_number__LineAdded_OK() {
  createBase alpha

  sed -i 's/ km_core_actions_dispose@Base 17.0.197/ km_core_actions_dispose@Base 17.0.197\n km_core_added@Base 17.0.255/' linux/debian/libkeymancore1.symbols
  git add linux/debian/libkeymancore1.symbols
  git commit -m "API method added"

  echo "## Calling API verification"
  pwd
  linux/scripts/deb-packaging.sh --bin-pkg "${BINPKG_NAME}" --git-sha "$(git rev-parse HEAD)" --git-base master verify
}

test_check_updated_version_number__LineAddedWithoutVerUpd_ERROR() {
  local output
  createBase alpha

  sed -i 's/ km_core_actions_dispose@Base 17.0.197/ km_core_actions_dispose@Base 17.0.197\n km_core_added@Base 17.0.197/' linux/debian/libkeymancore1.symbols
  git add linux/debian/libkeymancore1.symbols
  git commit -m "API method added"

  echo "## Calling API verification"
  pwd
  output=$(linux/scripts/deb-packaging.sh --bin-pkg "${BINPKG_NAME}" --git-sha "$(git rev-parse HEAD)" --git-base master verify || true)
  echo "${output[*]}" # for logging
  [[ "${output[*]}" == *"ERROR: libkeymancore1.symbols file got changed without changing the package version number of the symbol"* ]]
}

test_check_updated_version_number__LineRemovedWithAPIUpd_OK() {
  createBase alpha
  echo "2.0.0" > core/CORE_API_VERSION.md
  git add core/CORE_API_VERSION.md
  git mv linux/debian/libkeymancore{1,2}.symbols
  sed -i 's/libkeymancore1/libkeymancore2/' linux/debian/libkeymancore2.symbols
  sed -i 's/libkeymancore.so.1/libkeymancore.so.2/' linux/debian/libkeymancore2.symbols
  sed -i '6d' linux/debian/libkeymancore2.symbols
  git add linux/debian/libkeymancore2.symbols
  git commit -m "API method removed"

  echo "## Calling API verification"
  pwd
  linux/scripts/deb-packaging.sh --bin-pkg "${BINPKG_NAME}" --git-sha "$(git rev-parse HEAD)" --git-base master verify
}

test_check_updated_version_number__LineRemoved_OnlyCoreApiUpd_ERROR() {
  local output
  createBase alpha
  echo "2.0.0" > core/CORE_API_VERSION.md
  git add core/CORE_API_VERSION.md
  sed -i '6d' linux/debian/libkeymancore1.symbols
  git add linux/debian/libkeymancore1.symbols
  git commit -m "API method removed"

  echo "## Calling API verification"
  pwd
  output=$(linux/scripts/deb-packaging.sh --bin-pkg "${BINPKG_NAME}" --git-sha "$(git rev-parse HEAD)" --git-base master verify || true)
  echo "${output[*]}" # for logging
  [[ "${output[*]}" == *"ERROR: Missing libkeymancore2.symbols file"* ]]
}

test_check_updated_version_number__LineRemoved_OnlySymbolsFileUpd_ERROR() {
  local output
  createBase alpha
  git mv linux/debian/libkeymancore{1,2}.symbols
  sed -i 's/libkeymancore1/libkeymancore2/' linux/debian/libkeymancore2.symbols
  sed -i 's/libkeymancore.so.1/libkeymancore.so.2/' linux/debian/libkeymancore2.symbols
  sed -i '6d' linux/debian/libkeymancore2.symbols
  git add linux/debian/libkeymancore2.symbols
  git commit -m "API method removed"

  echo "## Calling API verification"
  pwd
  output=$(linux/scripts/deb-packaging.sh --bin-pkg "${BINPKG_NAME}" --git-sha "$(git rev-parse HEAD)" --git-base master verify || true)
  echo "${output[*]}" # for logging
  [[ "${output[*]}" == *"ERROR: Missing libkeymancore1.symbols file"* ]]
}

test_check_updated_version_number__LineRemovedWithAPIUpd_NotMetadataUpd_ERROR() {
  local output
  createBase alpha
  echo "2.0.0" > core/CORE_API_VERSION.md
  git add core/CORE_API_VERSION.md
  git mv linux/debian/libkeymancore{1,2}.symbols
  sed -i '6d' linux/debian/libkeymancore2.symbols
  git add linux/debian/libkeymancore2.symbols
  git commit -m "API method removed"

  echo "## Calling API verification"
  pwd
  output=$(linux/scripts/deb-packaging.sh --bin-pkg "${BINPKG_NAME}" --git-sha "$(git rev-parse HEAD)" --git-base master verify || true)
  echo "${output[*]}" # for logging
  [[ "${output[*]}" == *"ERROR: API version in .symbols file and in CORE_API_VERSION.md is different"* ]]
}

test_check_updated_version_number__LineRemoved_InAlpha_ChangedBefore_OK() {
  createBase alpha
  git checkout master
  # simulate a commit that already introduced an API version change
  echo "2.0.0" > core/CORE_API_VERSION.md
  git add core/CORE_API_VERSION.md
  git mv linux/debian/libkeymancore{1,2}.symbols
  sed -i 's/libkeymancore1/libkeymancore2/' linux/debian/libkeymancore2.symbols
  sed -i 's/libkeymancore.so.1/libkeymancore.so.2/' linux/debian/libkeymancore2.symbols
  git add linux/debian/libkeymancore2.symbols
  git commit -m "API version change"
  git checkout chore
  git rebase master

  sed -i '6d' linux/debian/libkeymancore2.symbols
  git add linux/debian/libkeymancore2.symbols
  git commit -m "API method removed"

  echo "## Calling API verification"
  pwd
  linux/scripts/deb-packaging.sh --bin-pkg "${BINPKG_NAME}" --git-sha "$(git rev-parse HEAD)" --git-base master verify
}

test_check_updated_version_number__LineRemoved_InAlpha_FileMissingInStable_ApiVerChanged_OK() {
  createBase alpha
  git checkout master
  # simulate a commit that renamed the .symbols file and updated the API version
  git mv linux/debian/libkeymancore1.symbols linux/debian/libfoo2.symbols
  sed -i 's/libkeymancore/libfoo/' linux/scripts/verify_api.inc.sh
  # shellcheck disable=2016 # single quotes are intentional here
  sed -i 's/${SONAME}/2/' linux/scripts/verify_api.inc.sh
  git add  linux/scripts/verify_api.inc.sh
  echo "2.0.0" > core/CORE_API_VERSION.md
  git add core/CORE_API_VERSION.md
  sed -i 's/libkeymancore1/libfoo2/' linux/debian/libfoo2.symbols
  sed -i 's/libkeymancore.so.1/libfoo.so.2/' linux/debian/libfoo2.symbols
  git add linux/debian/libfoo2.symbols
  git commit -m "renamed library"
  git checkout chore
  git rebase master

  sed -i '6d' linux/debian/libfoo2.symbols
  git add linux/debian/libfoo2.symbols
  git commit -m "API method removed"

  echo "## Calling API verification"
  pwd
  linux/scripts/deb-packaging.sh --bin-pkg "${BINPKG_NAME}" --git-sha "$(git rev-parse HEAD)" --git-base master verify
}

test_check_updated_version_number__LineRemoved_InAlpha_FileMissingInStable_ApiVerUnchanged_ERROR() {
  local output
  createBase alpha
  git checkout master
  # simulate a commit that renamed the .symbols file
  git mv linux/debian/libkeymancore1.symbols linux/debian/libfoo1.symbols
  sed -i 's/libkeymancore/libfoo/' linux/scripts/verify_api.inc.sh
  git add linux/scripts/verify_api.inc.sh
  sed -i 's/libkeymancore/libfoo/' linux/debian/libfoo1.symbols
  git add linux/debian/libfoo1.symbols
  git commit -m "renamed library"
  git checkout chore
  git rebase master

  sed -i '6d' linux/debian/libfoo1.symbols
  git add linux/debian/libfoo1.symbols
  git commit -m "API method removed"

  echo "## Calling API verification"
  pwd
  output=$(linux/scripts/deb-packaging.sh --bin-pkg "${BINPKG_NAME}" --git-sha "$(git rev-parse HEAD)" --git-base master verify || true)
  echo "${output[*]}" # for logging
  [[ "${output[*]}" == *" ERROR: Major API change without updating API version number in libfoo1.symbols file"* ]]
}

test_check_updated_version_number__LineRemoved_InAlpha_ChangeFromStable_ERROR() {
  local output
  createBase alpha

  sed -i '6d' linux/debian/libkeymancore1.symbols
  git add linux/debian/libkeymancore1.symbols
  git commit -m "API method removed"

  echo "## Calling API verification"
  pwd
  output=$(linux/scripts/deb-packaging.sh --bin-pkg "${BINPKG_NAME}" --git-sha "$(git rev-parse HEAD)" --git-base master verify || true)
  echo "${output[*]}" # for logging
  [[ "${output[*]}" == *" ERROR: Major API change without updating API version number in libkeymancore1.symbols file"* ]]
}

test_check_updated_version_number__LineRemoved_InBeta_ApiVerUnchanged_ERROR() {
  local output
  createBase beta

  # simulate a commit that already introduced an API version change in Beta
  git checkout -b beta
  echo "2.0.0" > core/CORE_API_VERSION.md
  git add core/CORE_API_VERSION.md
  git mv linux/debian/libkeymancore{1,2}.symbols
  sed -i 's/libkeymancore1/libkeymancore2/' linux/debian/libkeymancore2.symbols
  sed -i 's/libkeymancore.so.1/libkeymancore.so.2/' linux/debian/libkeymancore2.symbols
  git add linux/debian/libkeymancore2.symbols
  git commit -m "API version change"
  git checkout chore
  git rebase beta

  sed -i '6d' linux/debian/libkeymancore2.symbols
  git add linux/debian/libkeymancore2.symbols
  git commit -m "API method removed"

  echo "## Calling API verification"
  pwd
  output=$(linux/scripts/deb-packaging.sh --bin-pkg "${BINPKG_NAME}" --git-sha "$(git rev-parse HEAD)" --git-base beta verify || true)
  echo "${output[*]}" # for logging
  [[ "${output[*]}" == *" ERROR: Major API change without updating API version number in libkeymancore2.symbols file"* ]]
}

test_check_updated_version_number__LineRemoved_InBeta_ApiVerChanged_OK() {
  createBase beta

  # simulate a commit that already introduced an API version change in Beta
  git checkout -b beta
  echo "2.0.0" > core/CORE_API_VERSION.md
  git add core/CORE_API_VERSION.md
  git mv linux/debian/libkeymancore{1,2}.symbols
  sed -i 's/libkeymancore1/libkeymancore2/' linux/debian/libkeymancore2.symbols
  sed -i 's/libkeymancore.so.1/libkeymancore.so.2/' linux/debian/libkeymancore2.symbols
  git add linux/debian/libkeymancore2.symbols
  git commit -m "API version change"
  git checkout chore
  git rebase beta

  echo "3.0.0" > core/CORE_API_VERSION.md
  git add core/CORE_API_VERSION.md
  git mv linux/debian/libkeymancore{2,3}.symbols
  sed -i 's/libkeymancore2/libkeymancore3/' linux/debian/libkeymancore3.symbols
  sed -i 's/libkeymancore.so.2/libkeymancore.so.3/' linux/debian/libkeymancore3.symbols
  sed -i '6d' linux/debian/libkeymancore3.symbols
  git add linux/debian/libkeymancore3.symbols
  git commit -m "API method removed"

  echo "## Calling API verification"
  pwd
  linux/scripts/deb-packaging.sh --bin-pkg "${BINPKG_NAME}" --git-sha "$(git rev-parse HEAD)" --git-base beta verify
}

test_check_updated_version_number__LineRemoved_InBeta_FileMissingInStable_ApiVerUnchanged_ERROR() {
  local output
  createBase alpha
  git checkout -b beta
  # simulate a commit that renamed the .symbols file
  git mv linux/debian/libkeymancore1.symbols linux/debian/libfoo1.symbols
  sed -i 's/libkeymancore/libfoo/' linux/scripts/verify_api.inc.sh
  git add linux/scripts/verify_api.inc.sh
  sed -i 's/libkeymancore/libfoo/' linux/debian/libfoo1.symbols
  git add linux/debian/libfoo1.symbols
  git commit -m "renamed library"
  git checkout chore
  git rebase beta

  sed -i '6d' linux/debian/libfoo1.symbols
  git add linux/debian/libfoo1.symbols
  git commit -m "API method removed"

  echo "## Calling API verification"
  pwd
  output=$(linux/scripts/deb-packaging.sh --bin-pkg "${BINPKG_NAME}" --git-sha "$(git rev-parse HEAD)" --git-base beta verify || true)
  echo "${output[*]}" # for logging
  [[ "${output[*]}" == *" ERROR: Major API change without updating API version number in libfoo1.symbols file"* ]]
}

test_check_updated_version_number__LineInsertedInBranch_OK() {
  createBase alpha

  local base_sha=$(git rev-parse master)

  # Add a line in chore branch
  echo " km_core_foo@Base 17.0.200" >> linux/debian/libkeymancore1.symbols
  git add linux/debian/libkeymancore1.symbols
  git commit -m "API method added in chore branch"

  # Add a line in master branch
  git checkout master
  echo " km_core_foo@Base 17.0.205" >> linux/debian/libkeymancore1.symbols
  git add linux/debian/libkeymancore1.symbols
  git commit -m "API method changed in master branch"
  echo "readme" > README.md
  git add README.md
  git commit -m "Some change on master"
  git checkout chore

  # merge master into chore
  git merge --strategy-option=ours master

  echo "## Calling API verification"
  pwd
  linux/scripts/deb-packaging.sh --bin-pkg "${BINPKG_NAME}" --git-sha "$(git rev-parse HEAD)" --git-base "${base_sha}" verify
}

echo "(test logs are in /tmp/<testname>.log)"
run_test test_check_updated_version_number__NoChange_OK
run_test test_check_updated_version_number__LineAdded_OK
run_test test_check_updated_version_number__LineAddedWithoutVerUpd_ERROR
run_test test_check_updated_version_number__LineRemovedWithAPIUpd_OK
run_test test_check_updated_version_number__LineRemoved_OnlyCoreApiUpd_ERROR
run_test test_check_updated_version_number__LineRemoved_OnlySymbolsFileUpd_ERROR
run_test test_check_updated_version_number__LineRemovedWithAPIUpd_NotMetadataUpd_ERROR
run_test test_check_updated_version_number__LineRemoved_InAlpha_ChangedBefore_OK
run_test test_check_updated_version_number__LineRemoved_InAlpha_FileMissingInStable_ApiVerChanged_OK
run_test test_check_updated_version_number__LineRemoved_InAlpha_FileMissingInStable_ApiVerUnchanged_ERROR
run_test test_check_updated_version_number__LineRemoved_InAlpha_ChangeFromStable_ERROR
run_test test_check_updated_version_number__LineRemoved_InBeta_ApiVerUnchanged_ERROR
run_test test_check_updated_version_number__LineRemoved_InBeta_ApiVerChanged_OK
run_test test_check_updated_version_number__LineRemoved_InBeta_FileMissingInStable_ApiVerUnchanged_ERROR
run_test test_check_updated_version_number__LineInsertedInBranch_OK

# TODO: still some test cases missing for the different checks
