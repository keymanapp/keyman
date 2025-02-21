#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
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
  build       k_000___null_keyboard.kmx

do_configure() {
  mkdir -p kmcomp
  unzip kmcomp-16.0.138.zip -d kmcomp/
}

do_build() {
  local name
  for name in *.kmn; do
    ./kmcomp/kmcomp.exe -no-compiler-version -d "$name"
  done
}

builder_run_action clean      rm -rf *.kmx ./kmcomp/
builder_run_action configure  do_configure
builder_run_action build      do_build
