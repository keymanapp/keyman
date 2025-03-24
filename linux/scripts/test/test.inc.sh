#!/usr/bin/env bash

setup() {
  OLDPATH=${PATH}
  tmpDir="$(mktemp -d)"
  mockDebPkgTools
}

teardown() {
  PATH=${OLDPATH}
  rm -rf "${tmpDir}"
}

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
 km_core_context_get@Base 17.0.198
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

run_test() {
  setup
  $1 > "/tmp/$1.log" 2>&1 && echo -e "${COLOR_GREEN}$1: OK${COLOR_RESET}" || echo -e "${COLOR_RED}$1: FAILED${COLOR_RESET}"
  teardown
}
