#!/usr/bin/env bash
#
# Build Keyman Developer components
#
# TODO: convert existing Makefiles/ into build.sh scripts; for now,
# this only builds kmc and kmcmplib

#set -x
set -eu

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
  test \
  ":kmcmplib=src/kmcmplib                   Compiler - .kmn compiler" \
  ":kmc-kmn=src/kmc-kmn                     Compiler - .kmn wrapper Keyboard Module" \
  ":kmc-ldml=src/kmc-ldml                   Compiler - LDML Keyboard Module" \
  ":kmc-model=src/kmc-model                 Compiler - Lexical Model Module" \
  ":kmc-model-info=src/kmc-model-info       Compiler - .model_info Module" \
  ":kmc-package=src/kmc-package             Compiler - Package Module" \
  ":kmc=src/kmc                             Compiler - Command Line Interface"

builder_parse "$@"

builder_run_child_actions clean configure build test
