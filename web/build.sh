#!/usr/bin/env bash
#
# Compile keymanweb and copy compiled javascript and resources to output/embedded folder
#

# set -x
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

WORKING_DIRECTORY=`pwd`

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

# ################################ Main script ################################

# Definition of global compile constants

UI="app/ui"
WEB="app/web"
EMBEDDED="app/embed"
ENGINE="engine" # For now, a catch-all for all engine code.  This should be subdivided
                # once we have multiple engine modules within this project
BUILD_BASE="build"

DEBUG="debug"
RELEASE="release"
INTERMEDIATE="obj"

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

WEB_OUTPUT_NO_MINI="$(output_path $WEB $DEBUG)"
WEB_OUTPUT="$(output_path $WEB $RELEASE)"

EMBED_OUTPUT_NO_MINI="$(output_path $EMBEDDED $DEBUG)"
EMBED_OUTPUT="$(output_path $EMBEDDED $RELEASE)"

UI_OUTPUT_NO_MINI="$(output_path $UI $DEBUG)"
UI_OUTPUT="$(output_path $UI $RELEASE)"

SOURCE="src"

SENTRY_RELEASE_VERSION="release-$VERSION_WITH_TAG"

# Ensures that we rely first upon the local npm-based install of Typescript.
# (Facilitates automated setup for build agents.)
PATH="../node_modules/.bin:$PATH"

compiler="npm run tsc --"
compilecmd="$compiler"

PREDICTIVE_TEXT_SOURCE="../common/predictive-text/unit_tests/in_browser/resources/models/simple-trie.js"
PREDICTIVE_TEXT_OUTPUT="src/test/manual/web/prediction-ui/simple-en-trie.js"

builder_check_color "$@"

# TODO: for predictive-text, we only need :headless, perhaps we should be splitting modules?
# TODO: remove :tools once kmlmc is a dependency for test:module

DOC_WEB_PRODUCT="${BUILDER_TERM_START}:web${BUILDER_TERM_END} build product"
DOC_TEST_WEB="${BUILDER_TERM_START}test:web${BUILDER_TERM_END}"
DOC_BUILD_EMBED_WEB="${BUILDER_TERM_START}build:embed${BUILDER_TERM_END} and ${BUILDER_TERM_START}build:web${BUILDER_TERM_END}"
DOC_TEST_SYMBOL="actions -${BUILDER_TERM_START}test${BUILDER_TERM_END}, ${BUILDER_TERM_START}upload-symbols${BUILDER_TERM_END}"

builder_describe "Builds Keyman Engine for Web." \
  "@../common/web/keyman-version build:engine build:embed build:web build:ui" \
  "@../common/web/input-processor build:engine build:embed build:web" \
  "@tools/sourcemap-root build:engine build:embed build:web" \
  "clean" \
  "configure" \
  "build" \
  "test             Runs unit tests.  Only ${DOC_TEST_WEB} is currently defined"  \
  ":embed           Builds the configuration of Keyman Engine for Web used within the Keyman mobile apps" \
  ":engine          Builds common code used by other targets" \
  ":web             Builds the website-oriented configuration of Keyman Engine for Web" \
  ":ui              Builds the desktop UI modules used by the ${DOC_WEB_PRODUCT}" \
  ":samples         Builds sample & test pages found under /samples and /testing, but not the ${DOC_WEB_PRODUCT}" \
  ":tools           Builds related development + unit-test resources" \
  "--skip-minify    Skips any minification steps in the build" \
  "--all            Sets action to run on KMW's submodules as well if appropriate ($DOC_TEST_SYMBOL)"

# Possible TODO?
# "upload-symbols   Uploads build product to Sentry for error report symbolification.  Only defined for $DOC_BUILD_EMBED_WEB" \

builder_describe_outputs \
  configure         ../node_modules \
  configure:embed   ../node_modules \
  configure:web     ../node_modules \
  configure:ui      ../node_modules \
  configure:samples ../node_modules \
  configure:tools   ../node_modules \
  build:embed       $(output_path $EMBEDDED $RELEASE)/keyman.js \
  build:engine      $(output_path $ENGINE $INTERMEDIATE)/keymanweb.js \
  build:web         $(output_path $WEB $RELEASE)/keymanweb.js \
  build:ui          $(output_path $UI $RELEASE)/kmwuibutton.js \
  build:samples     $PREDICTIVE_TEXT_OUTPUT
# Deliberately excluding build:tools b/c its script provides the definitions.

builder_parse "$@"

#### Build utility methods + definitions ####

# Build products for each main target.
EMBED_TARGETS=( "keyman.js" )
WEB_TARGETS=( "keymanweb.js" )
UI_TARGETS=( "kmwuibutton.js" "kmwuifloat.js" "kmwuitoggle.js" "kmwuitoolbar.js" )

: ${CLOSURECOMPILERPATH:=../node_modules/google-closure-compiler-java}
: ${JAVA:=java}

minifier="$CLOSURECOMPILERPATH/compiler.jar"

# We'd love to add the argument --source_map_include_content for distribution in the future,
# but Closure doesn't include the TS sources properly at this time.
#
# `checkTypes` is blocked b/c TypeScript can perform our type checking... and it causes an error
# with TypeScript's `extend` implementation (it doesn't recognize a constructor without manual edits).
# We also get a global `this` warning from the same.
#
# `checkVars` is blocked b/c Closure will otherwise fail on TypeScript namespacing, as each original TS
# source file will redeclare the namespace variable, despite being merged into a single file post-compilation.
#
# `jsDocMissingType` prevents errors on type documentation Closure thinks is missing.  TypeScript may not
# have the same requirements, and we trust TypeScript over Closure.
minifier_warnings="--jscomp_error=* --jscomp_off=lintChecks --jscomp_off=unusedLocalVariables --jscomp_off=globalThis --jscomp_off=checkTypes --jscomp_off=checkVars --jscomp_off=jsdocMissingType --jscomp_off=uselessCode --jscomp_off=missingRequire --jscomp_off=strictMissingRequire"

# We use these to prevent Closure from auto-inserting its own polyfills.  Turns out, they can break in the
# WebView used by Android API 19, which our app still supported when written.  Unsure if it can still occur
# in Android API 21, our current minimum.
#
# Also, we currently apply all needed polyfills either manually or during TS compilation; we don't need the extra,
# excess code.
minifier_lang_specs="--language_in ECMASCRIPT5 --language_out ECMASCRIPT5"
minifycmd="$JAVA -jar $minifier --compilation_level WHITESPACE_ONLY $minifier_warnings --generate_exports $minifier_lang_specs"

readonly minifier
readonly minifycmd

minified_sourcemap_cleaner="tools/sourcemap-root"

# Fails the build if a specified file does not exist.
assert_exists ( ) {
  if ! [ -f $1 ]; then
    echo "Build failed:  expected file ${COLOR_GREY}$1${COLOR_RESET} is missing."
    exit 1
  fi
}

# $1 - base file name
# $2 - output path
# $3 - optimization level
# $4 - extra path info to add to minified sourcemap "sourceRoot" property.
# $5 - additional output wrapper
minify ( ) {
    if [ $# -ge 4 ]; then
        cleanerOptions="--suffix $4"
    else
        cleanerOptions=
    fi

    if [ $# -ge 5 ]; then
        wrapper=$5
    else
        wrapper="%output%"
    fi

    local INPUT="$1"
    local INPUT_FILE="$(basename $1)"
    local INPUT_DIR="$(dirname $1)"
    local INPUT_SOURCEMAP="$INPUT_DIR/$INPUT_FILE.map"
    local OUTPUT="$2"
    local OUTPUT_FILE="$(basename $2)"
    local OUTPUT_SOURCEMAP="$(dirname $2)/$OUTPUT_FILE.map"

    # --source_map_location_mapping - maps paths on INPUT source maps for consumption by Closure.
    # ../../.. => keymanapp, ../.. => keymanapp/keyman.  We have TS root sources on 'keyman'.
    $minifycmd --source_map_input "$INPUT|$INPUT_SOURCEMAP" \
        --create_source_map "$OUTPUT_SOURCEMAP" --source_map_include_content \
        --source_map_location_mapping "$INPUT_DIR|../../.." \
        --js "$INPUT" --compilation_level $3 \
        --js_output_file "$OUTPUT" --warning_level VERBOSE --output_wrapper "$wrapper
//# sourceMappingURL=$1.map"

    # Now to clean the source map.
    assert_exists "$OUTPUT"
    assert_exists "$OUTPUT_SOURCEMAP"

    # "Clean" the minified output sourcemaps.
    node $minified_sourcemap_cleaner "$INPUT_SOURCEMAP" "$OUTPUT_SOURCEMAP" $cleanerOptions
}

# Copies specified engine resources to the specified target's build output directories.
#
# ### Parameters
#
# * 1: `product`    the product's source path under src/
# * 2: `outputs`    an array of resource types to copy over
#
# ### Example
#
# ```bash
#   copy_resources app/web osk ui
# ```
copy_resources ( ) {
  local COMPILE_TARGET=$1
  shift

  local RESOURCES_TO_COPY=("$@")

  # We leave out $INTERMEDIATE here, as it's not a 'release' of any sort and
  # thus doesn't need to publish sources or resources.
  local CONFIGS=($DEBUG)

  if ! builder_has_option --skip-minify; then
    CONFIGS+=($RELEASE)
  fi

  echo

  for CONFIG in "${CONFIGS[@]}";
  do
    local CONFIG_OUT_PATH=$(output_path $COMPILE_TARGET $CONFIG)

    echo Copying resources to $CONFIG_OUT_PATH/src

    for RESOURCE in "${RESOURCES_TO_COPY[@]}";
    do
      mkdir -p "$CONFIG_OUT_PATH/$RESOURCE"
      mkdir -p "$CONFIG_OUT_PATH/src/resources/$RESOURCE"

      echo "- $SOURCE/resources/$RESOURCE/ => $CONFIG_OUT_PATH/$RESOURCE"
      cp -Rf $SOURCE/resources/$RESOURCE  $CONFIG_OUT_PATH/  >/dev/null
    done

    echo
  done
}

# Copies specified source folders corresponding to the specified target's build
# output directories.
#
# ### Parameters
#
# * 1: `product`    the product's source path under src/
# * 2: `outputs`    an array of src/ subfolders to copy over
#
# ### Example
#
# ```bash
#   copy_sources app/web engine resources/osk
# ```
copy_sources ( ) {
  local COMPILE_TARGET=$1
  shift

  local SOURCES_TO_COPY=("$@")

  # We leave out $INTERMEDIATE here, as it's not a 'release' of any sort and
  # thus doesn't need to publish sources or resources.
  CONFIGS=($DEBUG)

  if ! builder_has_option --skip-minify; then
    CONFIGS+=($RELEASE)
  fi

  for CONFIG in "${CONFIGS[@]}";
  do
    CONFIG_OUT_PATH=$(output_path $COMPILE_TARGET $CONFIG)
    echo Copying $COMPILE_TARGET sources to $CONFIG_OUT_PATH/src

    rm -rf $CONFIG_OUT_PATH/src
    mkdir -p $CONFIG_OUT_PATH/src
    echo $VERSION_PATCH > $CONFIG_OUT_PATH/src/version.txt

    for SOURCE_FOLDER in "${SOURCES_TO_COPY[@]}";
    do
      echo "- $SOURCE/$SOURCE_FOLDER/ => $CONFIG_OUT_PATH/src/$SOURCE_FOLDER/"
      mkdir -p $CONFIG_OUT_PATH/src/$SOURCE_FOLDER
      cp -Rf  $SOURCE/$SOURCE_FOLDER/    $CONFIG_OUT_PATH/src/$SOURCE_FOLDER
    done

    echo
  done
}

# Compiles compiled scripts from the first folder specified to the second folder specified.
#
# ### Parameters
#
# * 1: `src`      the folder containing scripts to be copied
# * 2: `dst`      the destination folder for the copy operation
# * 3: `scripts`  an array of filenames for expected compiled scripts
#
# ### Example
#
# ```bash
#   compile_and_minify build/app/web/obj build/app/web/debug keymanweb.js
# ```
copy_outputs ( ) {
  local src=$1
  local dst=$2

  shift
  shift

  local BASE_SCRIPTS=("$@")

  mkdir -p "$dst"

  for SCRIPTJS in "${BASE_SCRIPTS[@]}";
  do
    cp -Rf $src/$SCRIPTJS             $dst/
    cp -Rf $src/$SCRIPTJS.map         $dst/
  done
}

# Compiles all build products corresponding to the specified target.
#
# ### Parameters
#
# * 1: `product`    the product's source path under src/
#
# ### Example
#
# ```bash
#   compile app/embed
# ```
compile ( ) {
  if [ $# -lt 1 ]; then
    fail "Scripting error: insufficient argument count!"
  fi

  local COMPILE_TARGET=$1
  local COMPILED_INTERMEDIATE_PATH=$(output_path $COMPILE_TARGET $INTERMEDIATE)

  shift

  $compilecmd -b src/$COMPILE_TARGET -v

  echo $COMPILE_TARGET TypeScript compiled under $COMPILED_INTERMEDIATE_PATH
}

# Finalizes all build products corresponding to the specified target.
# This should be called after `compile` for all `app/` targets.
#
# ### Parameters
#
# * 1: `product`    the product's source path under src/
# * 2: `outputs`    an array of expected output script files for the build
#
# ### Example
#
# ```bash
#   compile app/embed
#   finalize app/embed keyman.js
# ```
finalize ( ) {
  if [ $# -lt 2 ]; then
    fail "Scripting error: insufficient argument count!"
  fi

  local COMPILE_TARGET=$1
  local COMPILED_INTERMEDIATE_PATH=$(output_path $COMPILE_TARGET $INTERMEDIATE)
  local DEBUG_OUT_PATH=$(output_path $COMPILE_TARGET $DEBUG)
  local RELEASE_OUT_PATH=$(output_path $COMPILE_TARGET $RELEASE)

  shift

  local OUTPUT_SCRIPTS=("$@")

  # START:  debug output

  mkdir -p "$DEBUG_OUT_PATH"
  copy_outputs $COMPILED_INTERMEDIATE_PATH $DEBUG_OUT_PATH "${OUTPUT_SCRIPTS[@]}"

  echo Compiled $COMPILE_TARGET debug version saved under $DEBUG_OUT_PATH: ${OUTPUT_SCRIPTS[*]}

  # START:  release output
  if ! builder_has_option --skip-minify; then
    for SCRIPT in "${OUTPUT_SCRIPTS[@]}";
    do
      minify $COMPILED_INTERMEDIATE_PATH/$SCRIPT $RELEASE_OUT_PATH/$SCRIPT SIMPLE_OPTIMIZATIONS
    done

    echo Compiled $COMPILE_TARGET release version saved under $RELEASE_OUT_PATH: ${OUTPUT_SCRIPTS[*]}
  else
    # The prior 'release' is now outdated:  delete it.
    rm -rf $RELEASE_OUT_PATH
  fi
}

#### Build action definitions ####

if builder_start_action configure; then
  verify_npm_setup

  if ! builder_has_option --skip-minify; then
    # NPM install is required for the file to be present.
    if ! [ -f $minifier ];
    then
      echo File $minifier does not exist:  have you set the environment variable \$CLOSURECOMPILERPATH?
      exit 1
    fi
  fi

  builder_finish_action success configure
fi

## Clean actions

# Possible issue:  there's no clear rule to `clean` the engine, which is auto-built
# by build:embed and build:web.
#
# Some sort of command to run ONLY for a general `clean` (no target specified) would
# be perfect for that, I think.

if builder_start_action clean:engine; then
  rm -rf "$(output_path $ENGINE)"
  builder_finish_action success clean:engine
fi

if builder_start_action clean:embed; then
  rm -rf "$(output_path $EMBEDDED)"
  builder_finish_action success clean:embed
fi

if builder_start_action clean:web; then
  rm -rf "$(output_path $WEB)"
  builder_finish_action success clean:web
fi

if builder_start_action clean:ui; then
  rm -rf "$(output_path $UI)"
  builder_finish_action success clean:ui
fi

if builder_start_action clean:samples; then
  rm -f $PREDICTIVE_TEXT_OUTPUT

  builder_finish_action success clean:samples
fi

if builder_start_action clean:tools; then
  tools/build.sh clean

  builder_finish_action success clean:tools
fi

## Build actions

# Adds a simple version 'header' when there's a main engine build product.
if builder_has_action build:embed || \
   builder_has_action build:web || \
   builder_has_action build:ui; then

  echo ""
  echo "${COLOR_PURPLE}Compiling version ${VERSION}${COLOR_RESET}"
fi

echo ""

if builder_start_action build:engine; then
  compile $ENGINE

  builder_finish_action success build:engine
fi

if builder_start_action build:embed; then
  compile $EMBEDDED
  finalize $EMBEDDED ${EMBED_TARGETS[@]}

  # The embedded version doesn't use UI modules.
  copy_resources $EMBEDDED osk
  copy_sources $EMBEDDED app/embed engine resources/osk

  builder_finish_action success build:embed

  # TODO:  handle this block somehow.

  # if [ $UPLOAD_EMBED_SENTRY = true ]; then  # upload-symbols:embed
  #   if [ $BUILD_DEBUG_EMBED = true ]; then
  #     ARTIFACT_FOLDER="release/unminified/embedded"
  #     pushd $EMBED_OUTPUT_NO_MINI
  #   else
  #     ARTIFACT_FOLDER="release/embedded"
  #     pushd $EMBED_OUTPUTs
  #   fi
  #   echo "Uploading to Sentry..."
  #   npm run sentry-cli -- releases files "$SENTRY_RELEASE_VERSION" upload-sourcemaps --strip-common-prefix $ARTIFACT_FOLDER --rewrite --ext js --ext map --ext ts || fail "Sentry upload failed."
  #   echo "Upload successful."
  #   popd
  # fi
  builder_finish_action success build:embed
fi

### -embed section complete.

if builder_start_action build:web; then
  compile $WEB
  finalize $WEB ${WEB_TARGETS[@]}

  # The testing pages need both osk & ui resources in the same place.
  copy_resources $WEB osk ui
  copy_sources $WEB app/web engine resources/osk

  builder_finish_action success build:web
fi

if builder_start_action build:ui; then
  compile $UI
  finalize $UI ${UI_TARGETS[@]}

  copy_resources $UI ui
  copy_sources $UI app/ui resources/ui

  builder_finish_action success build:ui
fi

if builder_start_action build:tools; then
  tools/build.sh
  builder_finish_action success build:tools
fi

if builder_start_action build:samples; then
  # Some test pages actually have build scripts.
  ./src/test/manual/embed/android-harness/build.sh  # is not yet builder-based.

  echo "Copying samples & test page resources..."
  # Should probably be changed into a build script for the `prediction-ui` test page.
  cp "${PREDICTIVE_TEXT_SOURCE}" "${PREDICTIVE_TEXT_OUTPUT}"

  # Which could then have a parallel script for `prediction-mtnt` that downloads + extracts
  # the current MTNT model.

  builder_finish_action success build:samples;
fi

if builder_start_action test:web; then
  if builder_has_option --all; then
    ./test.sh
  else
    ./test.sh :engine
  fi

  builder_finish_action success test:web
fi

# TODO:  handle the block below somehow.

# # We can only upload 'web' / 'native' artifacts after ALL are done compiling.
# if [ $UPLOAD_WEB_SENTRY = true ]; then
#     pushd $WEB_OUTPUT
#     echo "Uploading to Sentry..."
#     npm run sentry-cli -- releases files "$SENTRY_RELEASE_VERSION" upload-sourcemaps --strip-common-prefix release/web/ --rewrite --ext js --ext map --ext ts || fail "Sentry upload failed."
#     echo "Upload successful."
#     popd
# fi
