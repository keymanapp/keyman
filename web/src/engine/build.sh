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

MAIN=engine/main     # Covers all engine code, including submodules like those listed below.
DEVICEDETECT=engine/device-detect
DOMUTILS=engine/dom-utils
KEYBOARDCACHE=engine/keyboard-cache
ELEMENTWRAPPERS=engine/element-wrappers
OSK=engine/osk

BUILD_BASE=build

OUTPUT_DIR=obj

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
#   cp index.js "$(output_path app/web debug)/index.js"
# ```
#
# The block above would copy index.js into the build output folder for app/web's debug
# product.
#
# ``` bash
#   rm -rf "$(output_path app/web)"
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

builder_describe "Builds engine modules for Keyman Engine for Web (KMW)." \
  "@../../../common/web/keyman-version build:main" \
  "@../../../common/web/input-processor build:main" \
  "clean" \
  "configure" \
  "build" \
  ":device-detect     Subset used for device-detection " \
  ":dom-utils         A common subset of function used for DOM calculations, layout, etc" \
  ":element-wrappers  Subset used to integrate with website elements" \
  ":keyboard-cache    Subset used to collate keyboards and request them from the cloud" \
  ":main              Builds all common code used by KMW's app/-level targets" \
  ":osk               Builds the Web OSK module"

# Possible TODO?
# "upload-symbols   Uploads build product to Sentry for error report symbolification.  Only defined for $DOC_BUILD_EMBED_WEB" \

builder_describe_outputs \
  configure                  ../../../node_modules \
  configure:device-detect    ../../../node_modules \
  configure:dom-utils        ../../../node_modules \
  configure:element-wrappers ../../../node_modules \
  configure:keyboard-cache   ../../../node_modules \
  configure:main             ../../../node_modules \
  configure:osk              ../../../node_modules \
  build:device-detect        $(output_path $DEVICEDETECT $OUTPUT_DIR)/index.js \
  build:dom-utils            $(output_path $DOMUTILS $OUTPUT_DIR)/index.js \
  build:element-wrappers     $(output_path $ELEMENTWRAPPERS $OUTPUT_DIR)/index.js \
  build:keyboard-cache       $(output_path $KEYBOARDCACHE $OUTPUT_DIR)/index.js \
  build:main                 $(output_path $MAIN $OUTPUT_DIR)/keymanweb.js \
  build:osk                  $(output_path $OSK $OUTPUT_DIR)/index.js

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
  local COMPILED_OUTPUT_PATH="$(output_path $COMPILE_TARGET $OUTPUT_DIR)"

  $compilecmd -b src/$COMPILE_TARGET -v

  # COMPILE_TARGET entries are all prefixed with `engine`, so remove that.
  if [ -f "../$COMPILE_TARGET/build-bundler.js" ]; then
    pushd "../$COMPILE_TARGET"
    node "./build-bundler.js"
    popd

    # So... tsc does declaration-bundling on its own pretty well, at least for local development.
    $compilecmd --emitDeclarationOnly --outFile ./build/$COMPILE_TARGET/lib/index.d.ts -p src/$COMPILE_TARGET
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

if builder_start_action build:dom-utils; then
  compile $DOMUTILS

  builder_finish_action success build:dom-utils
fi

if builder_start_action build:element-wrappers; then
  compile $ELEMENTWRAPPERS

  builder_finish_action success build:element-wrappers
fi

if builder_start_action build:keyboard-cache; then
  compile $KEYBOARDCACHE

  builder_finish_action success build:keyboard-cache
fi

if builder_start_action build:osk; then
  compile $OSK

  builder_finish_action success build:osk
fi

if builder_start_action build:main; then
  compile $MAIN

  builder_finish_action success build:main
fi