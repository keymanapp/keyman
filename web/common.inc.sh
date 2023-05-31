#!/usr/bin/env bash
#

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
    builder_die "Scripting error: insufficient argument count!"
  fi

  local COMPILE_TARGET="$1"

  tsc -b "${KEYMAN_ROOT}/web/src/$COMPILE_TARGET" -v

  # COMPILE_TARGET entries are all prefixed with `engine`, so remove that.
  if [ -f "./build-bundler.js" ]; then
    node "./build-bundler.js"

    # So... tsc does declaration-bundling on its own pretty well, at least for local development.
    tsc --emitDeclarationOnly --outFile "${KEYMAN_ROOT}/web/build/$COMPILE_TARGET/lib/index.d.ts" -p "${KEYMAN_ROOT}/web/src/$COMPILE_TARGET"
  fi
}

# Runs all headless tests corresponding to the specified target.
# This should be called from the working directory of a child project's
# build script.
#
# ### Parameters
#
# * 1: `product`    the folder under src/test/auto/headless containing the
#                   child project's tests
#
# ### Example
#
# ```bash
#   # from engine/osk
#   test-headless osk
# ```
test-headless ( ) {
  TEST_OPTS=
  if builder_has_option --ci; then
    TEST_OPTS="--reporter mocha-teamcity-reporter"
  fi

  mocha --recursive "${KEYMAN_ROOT}/web/src/test/auto/headless/$1" $TEST_OPTS
}