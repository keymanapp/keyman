#!/usr/bin/env bash
#
# Build Keyman Developer components
#
# TODO: convert existing Makefiles/ into build.sh scripts; for now,
# this only builds kmc and kmcmplib

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

################################ Main script ################################

builder_describe \
  "Build Keyman Developer

  **NOTE:** this only builds kmc and kmcmplib for now (TODO on other modules)" \
  \
  clean \
  configure \
  build \
  "api                       analyze API and prepare API documentation" \
  test \
  ":utils=src/common/web/utils              Developer utils" \
  ":kmcmplib=src/kmcmplib                   Compiler - .kmn compiler" \
  ":kmc-analyze=src/kmc-analyze             Compiler - Analysis Tools" \
  ":kmc-keyboard-info=src/kmc-keyboard-info Compiler - .keyboard_info Module" \
  ":kmc-kmn=src/kmc-kmn                     Compiler - .kmn to .kmx and .js Keyboard Module" \
  ":kmc-ldml=src/kmc-ldml                   Compiler - LDML Keyboard Module" \
  ":kmc-model=src/kmc-model                 Compiler - Lexical Model Module" \
  ":kmc-model-info=src/kmc-model-info       Compiler - .model_info Module" \
  ":kmc-package=src/kmc-package             Compiler - Package Module" \
  ":kmc=src/kmc                             Compiler - Command Line Interface"

builder_parse "$@"

builder_describe_outputs \
  configure  /developer/src/tike/xml/layoutbuilder/keymanweb-osk.ttf

builder_run_action configure cp "$KEYMAN_ROOT/common/resources/fonts/keymanweb-osk.ttf" "$KEYMAN_ROOT/developer/src/tike/xml/layoutbuilder/"

builder_run_child_actions clean configure build test api

function build_api() {
  api-documenter markdown -i ./build/api -o ./build/docs
  # TODO: Copy to help.keyman.com and open PR
}

builder_run_action api build_api
