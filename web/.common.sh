#!/usr/bin/env bash
#

#### Build utility methods + definitions ####

compiler="npm run tsc --"
compilecmd="$compiler"

# Compiles all build products corresponding to the specified target.
# This should be called from the working directory of a child project's
# build script.
#
# ### Parameters
#
# * 1: `product`    the product's source path under src/
#
# ### Example
#
# ```bash
#   compile engine/main
# ```
compile ( ) {
  if [ $# -lt 1 ]; then
    fail "Scripting error: insufficient argument count!"
  fi

  local COMPILE_TARGET=$1

  $compilecmd -b src/$COMPILE_TARGET -v

  # COMPILE_TARGET entries are all prefixed with `engine`, so remove that.
  if [ -f "./build-bundler.js" ]; then
    node "./build-bundler.js"

    # So... tsc does declaration-bundling on its own pretty well, at least for local development.
    $compilecmd --emitDeclarationOnly --outFile ./build/$COMPILE_TARGET/lib/index.d.ts -p src/$COMPILE_TARGET
  fi
}