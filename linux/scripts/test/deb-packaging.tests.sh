#!/bin/bash
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

mockDebPkgTools() {
  echo "#!/bin/bash
  " > ${tmpDir}/dpkg
  chmod +x ${tmpDir}/dpkg
  cp ${tmpDir}/dpkg ${tmpDir}/dpkg-gensymbols
  PATH=${tmpDir}:${PATH}
}

createBase() {
  TIER=$1
  workDir=$(mktemp -d)
  cd ${workDir}
  git init .
  mkdir -p linux/debian
  echo "libkeymancore.so.1 libkeymancore #MINVER#
* Build-Depends-Package: libkeymancore-dev

 km_core_actions_dispose@Base 17.0.197
 km_core_context_clear@Base 17.0.195
 km_core_context_get@Base 17.0.195
 km_core_context_item_list_size@Base 17.0.195
 km_core_context_items_dispose@Base 17.0.195
" > linux/debian/libkeymancore.symbols
  git add linux/debian/libkeymancore.symbols

  mkdir -p core
  echo "1.0.0" > core/CORE_API_VERSION.md
  git add core/CORE_API_VERSION.md

  echo "16.0.145" > VERSION.md
  git add VERSION.md

  echo "stable" > TIER.md
  git add TIER.md

  mkdir -p linux/scripts
  cp -r ${REPO_ROOT}/linux/scripts/* linux/scripts
  git add linux/scripts

  mkdir -p resources/build
  cp -r ${REPO_ROOT}/resources/build/* resources/build
  cp ${REPO_ROOT}/resources/builder.inc.sh resources/
  git add resources

  git commit -m "Initial"

  git branch -c stable-16.0
  git tag -m "16.0.145" release-16.0.145

  echo "${TIER}" > TIER.md
  git add TIER.md

  echo "17.0.255" > VERSION.md
  git add VERSION.md
  git commit -m "Commit for Alpha"

  git checkout -b chore

  BINPKG_NAME=${tmpDir}/libkeymancore_17.0.257-1+noble1_amd64.deb
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

test_check_updated_version_number_NoChange_OK() {
  createBase alpha

  echo "readme" > README.md
  git add README.md
  git commit -m "Some change on the branch"

  pwd
  linux/scripts/deb-packaging.sh --bin-pkg "${BINPKG_NAME}" --git-sha "$(git rev-parse HEAD)" --git-base master verify
}

test_check_updated_version_number_LineAdded_OK() {
  createBase alpha

  sed -i 's/ km_core_actions_dispose@Base 17.0.197/ km_core_actions_dispose@Base 17.0.197\n km_core_added@Base 17.0.255/' linux/debian/libkeymancore.symbols
  git add linux/debian/libkeymancore.symbols
  git commit -m "API method added"

  pwd
  linux/scripts/deb-packaging.sh --bin-pkg "${BINPKG_NAME}" --git-sha "$(git rev-parse HEAD)" --git-base master verify
}

test_check_updated_version_number_LineAddedWithoutVerUpd_ERROR() {
  createBase alpha

  sed -i 's/ km_core_actions_dispose@Base 17.0.197/ km_core_actions_dispose@Base 17.0.197\n km_core_added@Base 17.0.197/' linux/debian/libkeymancore.symbols
  git add linux/debian/libkeymancore.symbols
  git commit -m "API method added"

  pwd
  output=$(linux/scripts/deb-packaging.sh --bin-pkg "${BINPKG_NAME}" --git-sha "$(git rev-parse HEAD)" --git-base master verify || true)
  echo "${output[*]}" # for logging
  [[ "${output[*]}" == *"ERROR: libkeymancore.symbols file got changed without changing the package version number of the symbol"* ]]
}

test_check_updated_version_number_LineRemovedWithAPIUpd_OK() {
  createBase alpha
  echo "2.0.0" > core/CORE_API_VERSION.md
  git add core/CORE_API_VERSION.md
  sed -i 's/libkeymancore.so.1/libkeymancore.so.2/' linux/debian/libkeymancore.symbols
  sed -i '6d' linux/debian/libkeymancore.symbols
  git add linux/debian/libkeymancore.symbols
  git commit -m "API method removed"

  pwd
  linux/scripts/deb-packaging.sh --bin-pkg "${BINPKG_NAME}" --git-sha "$(git rev-parse HEAD)" --git-base master verify
}

test_check_updated_version_number_LineRemoved_InAlpha_OK() {
  createBase alpha
  git checkout master
  # simulate a commit that already introduced an API version change
  echo "2.0.0" > core/CORE_API_VERSION.md
  git add core/CORE_API_VERSION.md
  sed -i 's/libkeymancore.so.1/libkeymancore.so.2/' linux/debian/libkeymancore.symbols
  git add linux/debian/libkeymancore.symbols
  git commit -m "API version change"
  git checkout chore
  git rebase master

  sed -i '6d' linux/debian/libkeymancore.symbols
  git add linux/debian/libkeymancore.symbols
  git commit -m "API method removed"

  pwd
  linux/scripts/deb-packaging.sh --bin-pkg "${BINPKG_NAME}" --git-sha "$(git rev-parse HEAD)" --git-base master verify
}

test_check_updated_version_number_LineRemoved_ChangeFromStable_ERROR() {
  createBase alpha

  sed -i '6d' linux/debian/libkeymancore.symbols
  git add linux/debian/libkeymancore.symbols
  git commit -m "API method removed"

  pwd
  output=$(linux/scripts/deb-packaging.sh --bin-pkg "${BINPKG_NAME}" --git-sha "$(git rev-parse HEAD)" --git-base master verify || true)
  echo "${output[*]}" # for logging
  [[ "${output[*]}" == *" ERROR: Major API change without updating API version number in libkeymancore.symbols file"* ]]
}

test_check_updated_version_number_LineRemoved_ChangeInBeta_ERROR() {
  createBase beta

  # simulate a commit that already introduced an API version change in Beta
  git checkout -b beta
  echo "2.0.0" > core/CORE_API_VERSION.md
  git add core/CORE_API_VERSION.md
  sed -i 's/libkeymancore.so.1/libkeymancore.so.2/' linux/debian/libkeymancore.symbols
  git add linux/debian/libkeymancore.symbols
  git commit -m "API version change"
  git checkout chore
  git rebase beta

  sed -i '6d' linux/debian/libkeymancore.symbols
  git add linux/debian/libkeymancore.symbols
  git commit -m "API method removed"

  pwd
  output=$(linux/scripts/deb-packaging.sh --bin-pkg "${BINPKG_NAME}" --git-sha "$(git rev-parse HEAD)" --git-base beta verify || true)
  echo "${output[*]}" # for logging
  [[ "${output[*]}" == *" ERROR: Major API change without updating API version number in libkeymancore.symbols file"* ]]
}

echo "(test logs are in /tmp/<testname>.log)"
run_test test_check_updated_version_number_NoChange_OK
run_test test_check_updated_version_number_LineAdded_OK
run_test test_check_updated_version_number_LineAddedWithoutVerUpd_ERROR
run_test test_check_updated_version_number_LineRemovedWithAPIUpd_OK
run_test test_check_updated_version_number_LineRemoved_InAlpha_OK
run_test test_check_updated_version_number_LineRemoved_ChangeFromStable_ERROR
run_test test_check_updated_version_number_LineRemoved_ChangeInBeta_ERROR

# TODO: still some test cases missing for the different checks
