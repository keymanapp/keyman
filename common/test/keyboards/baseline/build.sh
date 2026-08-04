#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Baseline keyboards tests -- built with 16.0 compiler" \
  clean \
  configure \
  build

builder_describe_platform \
  :project  win

builder_parse "$@"

builder_describe_outputs \
  configure   kmcomp/kmcomp.exe \
  build       k_0000___null_keyboard.kmx

do_configure() {
  mkdir -p kmcomp
  unzip kmcomp-16.0.138.zip -d kmcomp/
}

do_build() {
  local name jsname
  for name in *.kmn; do
    jsname="$(basename "${name%.kmn}").js"
    ./kmcomp/kmcomp.exe -no-compiler-version -d "${name}"
    if [[ "${name}" != "k_0812___nul_and_contextex.kmn" ]]; then
      # k_0812___nul_and_contextex.kmn fails to compile because contextex
      # is not currently supported in context() match, so we skip it
      ./kmcomp/kmcomp.exe -no-compiler-version -d "${name}" "${jsname}"
    fi
  done
}

builder_run_action clean      rm -rf *.kmx ./kmcomp/
builder_run_action configure  do_configure
builder_run_action build      do_build
