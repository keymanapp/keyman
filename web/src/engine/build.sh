#!/usr/bin/env bash
#
# Compile keymanweb and copy compiled javascript and resources to output/embedded folder
#

# set -x
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

WORKING_DIRECTORY=`pwd`

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

# ################################ Main script ################################

# Definition of global compile constants

MAIN="engine/main" # Covers all engine code, including submodules like those listed below.
DEVICEDETECT="engine/device-detect"
ELEMENTWRAPPERS="engine/element-wrappers"

BUILD_BASE="build"

OUTPUT_DIR="obj"

# Composites and outputs the output path corresponding to the build configuration
# specified by the parameters.
#
# ### Parameters
#
# * 1: - build product (app/embed, app/web, app/ui, engine)
# * 2: (optional) - build stage / config (obj, debug, release)
#
# ### Example
#
# ```bash
#   cp index.js $(output_path app/web debug)/index.js
# ```
#
# The block above would copy index.js into the build output folder for app/web's debug
# product.
#
# ``` bash
#   rm -rf $(output_path app/web)
# ```
#
# The block above is useful for deleting all app/web build products as part of a `clean`
# action.
#
# ### Other Notes
#
# In the future, we may opt to move $INTERMEDIATE stuff underneath both $DEBUG and $RELEASE,
# making it a third param.  This is currently unclear, but if so, we'd do
# $DEBUG/$INTERMEDIATE and $RELEASE/$INTERMEDIATE via a third argument.
output_path ( ) {
  if [ $# -lt 1 ]; then
    echo "Insufficient argument count!"
    exit 1
  elif [ $# -eq 1 ]; then
    # Used by clean:<target> actions
    echo "$BUILD_BASE/$1"
  else
    echo "$BUILD_BASE/$1/$2"
  fi
}

SOURCE="src"

# Ensures that we rely first upon the local npm-based install of Typescript.
# (Facilitates automated setup for build agents.)
PATH="../../../node_modules/.bin:$PATH"

compiler="npm run tsc --"
compilecmd="$compiler"

builder_check_color "$@"

# TODO: for predictive-text, we only need :headless, perhaps we should be splitting modules?
# TODO: remove :tools once kmlmc is a dependency for test:module

DOC_WEB_PRODUCT="${BUILDER_TERM_START}:web${BUILDER_TERM_END} build product"
DOC_TEST_WEB="${BUILDER_TERM_START}test:web${BUILDER_TERM_END}"
DOC_BUILD_EMBED_WEB="${BUILDER_TERM_START}build:embed${BUILDER_TERM_END} and ${BUILDER_TERM_START}build:web${BUILDER_TERM_END}"
DOC_TEST_SYMBOL="actions - ${BUILDER_TERM_START}test${BUILDER_TERM_END}"

builder_describe "Builds engine modules for Keyman Engine for Web (KMW)." \
  "@../../../common/web/keyman-version build:main" \
  "@../../../common/web/input-processor build:main" \
  "clean" \
  "configure" \
  "build" \
  ":device-detect     Subset used for device-detection " \
  ":element-wrappers  Subset used to integrate with website elements" \
  ":main              Builds all common code used by KMW's app/-level targets"

# Possible TODO?
# "upload-symbols   Uploads build product to Sentry for error report symbolification.  Only defined for $DOC_BUILD_EMBED_WEB" \

builder_describe_outputs \
  configure                  ../node_modules \
  configure:device-detect    ../node_modules \
  configure:element-wrappers ../node_modules \
  configure:main             ../node_modules \
  build:device-detect        $(output_path $DEVICEDETECT $OUTPUT_DIR)/index.js \
  build:element-wrappers     $(output_path $ELEMENTWRAPPERS $OUTPUT_DIR)/index.js \
  build:main                 $(output_path $MAIN $OUTPUT_DIR)/keymanweb.js

builder_parse "$@"

#### Build utility methods + definitions ####

# Compiles all build products corresponding to the specified target.
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
  local COMPILED_OUTPUT_PATH=$(output_path $COMPILE_TARGET $OUTPUT_DIR)

  BUNDLE_CONFIG=src/$COMPILE_TARGET/tsconfig.bundled.json

  $compilecmd -b src/$COMPILE_TARGET -v

  # Handle any 'bundled' compilations, too!
  # `if`'s working dir:  web/src/engine; $compilecmd's: web/
  if [ -f "../../$BUNDLE_CONFIG" ]; then
    $compilecmd -b $BUNDLE_CONFIG -v
  fi

  echo $COMPILE_TARGET TypeScript compiled under $COMPILED_OUTPUT_PATH
}

#### Build action definitions ####

if builder_start_action configure; then
  verify_npm_setup

  builder_finish_action success configure
fi

## Clean actions

# Possible issue:  there's no clear rule to `clean` the engine, which is auto-built
# by build:embed and build:web.
#
# Some sort of command to run ONLY for a general `clean` (no target specified) would
# be perfect for that, I think.

if builder_start_action clean:device-detect; then
  rm -rf "$(output_path $DEVICEDETECT)"
  builder_finish_action success clean:device-detect
fi

if builder_start_action clean:element-wrappers; then
  rm -rf "$(output_path $ELEMENTWRAPPERS)"
  builder_finish_action success clean:element-wrappers
fi

if builder_start_action clean:main; then
  rm -rf "$(output_path $ENGINE)"
  builder_finish_action success clean:main
fi

## Build actions

# Adds a simple version 'header' when there's a main engine build product.
if builder_start_action build:device-detect; then
  compile $DEVICEDETECT

  builder_finish_action success build:device-detect
fi

if builder_start_action build:element-wrappers; then
  compile $ELEMENTWRAPPERS

  builder_finish_action success build:element-wrappers
fi

if builder_start_action build:main; then
  compile $MAIN

  builder_finish_action success build:main
fi