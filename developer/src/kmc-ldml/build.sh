#!/usr/bin/env bash
#
# Compiles the kmc keyboard compiler.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

builder_describe "Keyman kmc Keyboard Compiler module" \
  "@/common/web/keyman-version" \
  "@/common/web/types" \
  "@/developer/src/kmc-kmn" \
  "@/developer/src/common/web/utils" \
  "@/developer/src/common/web/test-helpers" \
  "configure" \
  "build" \
  "api                       analyze API and prepare API documentation" \
  "clean" \
  "test" \
  "build-fixtures            builds test fixtures for manual examination" \
  "publish                   publish to npm" \
  "--npm-publish+            For publish, do a npm publish, not npm pack (only for CI)" \
  "--dry-run,-n              don't actually publish, just dry run"

builder_describe_outputs \
  configure     /developer/src/kmc-ldml/src/util/abnf/46/transform-from-required.js \
  build         /developer/src/kmc-ldml/build/src/main.js \
  api           /developer/build/api/kmc-ldml.api.json

builder_parse "$@"

function do_clean() {
  rm -rf ./build/ ./tsconfig.tsbuildinfo ./src/util/abnf/*/*.pegjs ./src/util/abnf/*/*.ts ./src/util/abnf/*/*.js
}

function do_configure() {
  verify_npm_setup
  do_build_abnf
}

function do_build() {
  npm run build
}

function do_build_abnf() {
  # we convert over all abnf files found. 
  for file in "$KEYMAN_ROOT/resources/standards-data/ldml-keyboards"/*/abnf/*.abnf; do
    cldrver=$(basename $(dirname $(dirname "$file")))
    base=$(basename "$file" .abnf)
    peg="$base.pegjs"
    outdir="./src/util/abnf/$cldrver"
    outfile="$outdir/$peg"
    outjs="$outdir/$base.js"
    if [ ! -f "$outjs" ]; then
      mkdir -p "$outdir"
      printf "${COLOR_GREY}abnf_gen ${COLOR_PURPLE}${cldrver}/${base}.abnf -> ${peg}${COLOR_RESET}\n"
      "$KEYMAN_ROOT/node_modules/.bin/abnf_gen"  "$file" -o "$outfile"
      printf "${COLOR_GREY}peggy ${COLOR_PURPLE}${cldrver}/${peg} -> ${base}.js${COLOR_RESET}\n"
      "$KEYMAN_ROOT/node_modules/.bin/peggy" "$outfile" -o "$outjs" --format es --dts
    fi
  done
}

function do_build_fixtures() {
  # Build basic.kmx and emit its checksum
  mkdir -p ./build/test/fixtures
  node ../kmc build ./test/fixtures/basic.xml --no-compiler-version --debug --out-file ./build/test/fixtures/basic-xml.kmx
  printf "${COLOR_GREY}Checksum for basic-xml.kmx: ${COLOR_PURPLE}%s${COLOR_RESET}\n" \
    "$(xxd -g 1 -l 12 ./build/test/fixtures/basic-xml.kmx | cut -d' ' -f 10-13)"

  # Generate a binary file from basic.txt for comparison purposes
  node ../../../common/tools/hextobin/build/hextobin.js ./test/fixtures/basic.txt ./build/test/fixtures/basic-txt.kmx
}

builder_run_action clean           do_clean
builder_run_action configure       do_configure
builder_run_action build           do_build
builder_run_action build-fixtures  do_build_fixtures
builder_run_action api             api-extractor run --local --verbose
builder_run_action test            builder_do_typescript_tests

#-------------------------------------------------------------------------------------------------------------------

. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"

builder_run_action publish     builder_publish_npm
