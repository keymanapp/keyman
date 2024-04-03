#!/bin/bash
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

mockDebPkgTools() {
  echo "#!/bin/bash
  " > "${tmpDir}/dpkg"
  chmod +x "${tmpDir}/dpkg"
  cp "${tmpDir}/dpkg" "${tmpDir}/dpkg-gensymbols"
  PATH=${tmpDir}:${PATH}
}

createBase() {
  TIER=$1
  remoteDir=$(mktemp -d)
  cd "${remoteDir}"
  git init --bare --initial-branch=master .

  workDir=$(mktemp -d)
  cd "${workDir}"
  git init --initial-branch=master .
  git remote add origin "${remoteDir}"
  mkdir -p linux/debian
  echo "libkeymancore.so.1 libkeymancore1 #MINVER#
* Build-Depends-Package: libkeymancore-dev

 km_core_actions_dispose@Base 17.0.197
 km_core_context_clear@Base 17.0.195
 km_core_context_get@Base 17.0.195
 km_core_context_item_list_size@Base 17.0.195
 km_core_context_items_dispose@Base 17.0.195
" > linux/debian/libkeymancore1.symbols
  git add linux/debian/libkeymancore1.symbols

  mkdir -p core
  echo "1.0.0" > core/CORE_API_VERSION.md
  git add core/CORE_API_VERSION.md

  echo "16.0.145" > VERSION.md
  git add VERSION.md

  echo "stable" > TIER.md
  git add TIER.md

  mkdir -p linux/scripts
  cp -r "${REPO_ROOT}"/linux/scripts/* linux/scripts
  git add linux/scripts

  mkdir -p resources/build
  cp -r "${REPO_ROOT}"/resources/build/* resources/build
  cp "${REPO_ROOT}"/resources/builder.inc.sh resources/
  git add resources

  git commit -m "Initial"
  git push origin master

  git branch -c stable-16.0
  git push origin stable-16.0
  git tag -m "16.0.145" release-16.0.145
  git push origin release-16.0.145

  echo "${TIER}" > TIER.md
  git add TIER.md

  echo "17.0.255" > VERSION.md
  git add VERSION.md
  git commit -m "Commit for Alpha"
  git push origin master

  git checkout -b chore

  BINPKG_NAME=${tmpDir}/libkeymancore1_17.0.257-1+noble1_amd64.deb
  touch "${BINPKG_NAME}"
}

setup() {
  OLDPATH=${PATH}
  tmpDir=$(mktemp -d)
  mockDebPkgTools
}

teardown() {
  PATH=${OLDPATH}
  rm -rf ${tmpDir}
}

run_test() {
  setup
  $1 > /tmp/$1.log 2>&1 && echo -e "${COLOR_GREEN}$1: OK${COLOR_RESET}" || echo -e "${COLOR_RED}$1: FAILED${COLOR_RESET}"
  teardown
}

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
  sed -i 's/libkeymancore/libfoo/' linux/scripts/deb-packaging.sh
  # shellcheck disable=2016 # single quotes are intentional here
  sed -i 's/${SONAME}/2/' linux/scripts/deb-packaging.sh
  git add  linux/scripts/deb-packaging.sh
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
  createBase alpha
  git checkout master
  # simulate a commit that renamed the .symbols file
  git mv linux/debian/libkeymancore1.symbols linux/debian/libfoo1.symbols
  sed -i 's/libkeymancore/libfoo/' linux/scripts/deb-packaging.sh
  git add  linux/scripts/deb-packaging.sh
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
  createBase alpha
  git checkout -b beta
  # simulate a commit that renamed the .symbols file
  git mv linux/debian/libkeymancore1.symbols linux/debian/libfoo1.symbols
  sed -i 's/libkeymancore/libfoo/' linux/scripts/deb-packaging.sh
  git add linux/scripts/deb-packaging.sh
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

# TODO: still some test cases missing for the different checks
