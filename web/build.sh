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
UI="ui"
WEB="web"
EMBEDDED="embedded"

WEB_OUTPUT="release/web"
EMBED_OUTPUT="release/embedded"
WEB_OUTPUT_NO_MINI="release/unminified/web"
EMBED_OUTPUT_NO_MINI="release/unminified/embedded"
INTERMEDIATE="intermediate"
SOURCE="src"

SENTRY_RELEASE_VERSION="release-$VERSION_WITH_TAG"

readonly WEB_OUTPUT
readonly EMBED_OUTPUT
readonly SOURCE

# Ensures that we rely first upon the local npm-based install of Typescript.
# (Facilitates automated setup for build agents.)
PATH="../node_modules/.bin:$PATH"

compiler="npm run tsc --"
compilecmd="$compiler"

PREDICTIVE_TEXT_SOURCE="../common/predictive-text/unit_tests/in_browser/resources/models/simple-trie.js"
PREDICTIVE_TEXT_OUTPUT="testing/prediction-ui/simple-en-trie.js"

builder_check_color "$@"

# TODO: for predictive-text, we only need :headless, perhaps we should be splitting modules?
# TODO: remove :tools once kmlmc is a dependency for test:module

DOC_WEB_PRODUCT="${BUILDER_TERM_START}:web${BUILDER_TERM_END} build product"
DOC_TEST_WEB="${BUILDER_TERM_START}test:web${BUILDER_TERM_END}"
DOC_BUILD_EMBED_WEB="${BUILDER_TERM_START}build:embed${BUILDER_TERM_END} and ${BUILDER_TERM_START}build:web${BUILDER_TERM_END}"
DOC_TEST_SYMBOL="actions -${BUILDER_TERM_START}test${BUILDER_TERM_END}, ${BUILDER_TERM_START}upload-symbols${BUILDER_TERM_END}"

builder_describe "Builds Keyman Engine for Web." \
  "@../common/web/keyman-version build:embed build:web build:ui" \
  "@../common/web/input-processor build:embed build:web" \
  "@../common/web/lm-worker build:embed build:web" \
  "@tools/sourcemap-root build:embed build:web" \
  "clean" \
  "configure" \
  "build" \
  "test             Runs unit tests.  Only ${DOC_TEST_WEB} is currently defined"  \
  ":embed           Builds the configuration of Keyman Engine for Web used within the Keyman mobile apps" \
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
  build:embed       $EMBED_OUTPUT/keyman.js \
  build:web         $WEB_OUTPUT/keymanweb.js \
  build:ui          $WEB_OUTPUT/kmwuibutton.js \
  build:samples     $PREDICTIVE_TEXT_OUTPUT
# Deliberately excluding build:tools b/c its script provides the definitions.

builder_parse "$@"

#### Build utility methods + definitions ####

# Build products for each main target.
WEB_TARGET=( "keymanweb.js" )
UI_TARGET=( "kmwuibutton.js" "kmwuifloat.js" "kmwuitoggle.js" "kmwuitoolbar.js" )
EMBED_TARGET=( "keyman.js" )

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
# WebView used by Android API 19, which our app still supports.
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

    local INPUT="$INTERMEDIATE/$1"
    local INPUT_SOURCEMAP="$INPUT.map"
    local OUTPUT="$2/$1"
    local OUTPUT_SOURCEMAP="$OUTPUT.map"

    # --source_map_location_mapping - maps paths on INPUT source maps for consumption by Closure.
    # ../../.. => keymanapp, ../.. => keymanapp/keyman.  We have TS root sources on 'keyman'.
    $minifycmd --source_map_input "$INPUT|$INPUT_SOURCEMAP" \
        --create_source_map "$OUTPUT_SOURCEMAP" --source_map_include_content \
        --source_map_location_mapping "$INTERMEDIATE|../../.." \
        --js "$INPUT" --compilation_level $3 \
        --js_output_file "$OUTPUT" --warning_level VERBOSE --output_wrapper "$wrapper
//# sourceMappingURL=$1.map"

    # Now to clean the source map.
    assert_exists "$OUTPUT"
    assert_exists "$OUTPUT_SOURCEMAP"

    # "Clean" the minified output sourcemaps.
    node $minified_sourcemap_cleaner "$INPUT_SOURCEMAP" "$OUTPUT_SOURCEMAP" $cleanerOptions
}

# $1 - target (WEB, EMBEDDED)
# $2+ - build target array (one of WEB_TARGET, FULL_WEB_TARGET, or EMBED_TARGET)
finish_nominify ( ) {
    args=("$@")
    if [ $1 = $WEB ] || [ $1 = $UI ]; then
        dest=$WEB_OUTPUT_NO_MINI
        resourceDest=$dest
    else
        dest=$EMBED_OUTPUT_NO_MINI
        resourceDest=$dest/resources
    fi

    # Create our entire embedded compilation results path.
    if ! [ -d $dest ]; then
        mkdir -p "$dest"  # Includes base folder, is recursive.
    fi
    echo "Copying unminified $1 build products to $dest."

    for (( n=1; n<$#; n++ ))  # Apparently, args ends up zero-based, meaning $2 => n=1.
    do
        target=${args[$n]}
        cp -f $INTERMEDIATE/$target $dest/$target
        cp -f $INTERMEDIATE/$target.map $dest/$target.map
    done

    copy_resources $resourceDest
}

copy_resources ( ) {
    echo
    echo Copy resources to $1/ui, .../osk

    # Create our entire compilation results path.  Can't one-line them due to shell-script parsing errors.
    if ! [ -d $1/ui ];      then
        mkdir -p "$1/ui"
    fi
    if ! [ -d $1/osk ];     then
        mkdir -p "$1/osk"
    fi
    if ! [ -d $1/src/ui ];  then
        mkdir -p "$1/src/ui"
    fi
    if ! [ -d $1/src/osk ]; then
        mkdir -p "$1/src/osk"
    fi

    cp -Rf $SOURCE/resources/ui  $1/  >/dev/null
    cp -Rf $SOURCE/resources/osk $1/  >/dev/null

    echo Copy source to $1/src
    cp -Rf $SOURCE/*.js $1/src
    cp -Rf $SOURCE/*.ts $1/src
    echo $VERSION_PATCH > $1/src/version.txt

    # Remove KMW Recorder source.
    rm -f $1/src/recorder_*.ts
    rm -f $1/src/recorder_*.js

    cp -Rf $SOURCE/resources/ui  $1/src/ >/dev/null
    cp -Rf $SOURCE/resources/osk $1/src/ >/dev/null

    # Update build number if successful
    echo
    echo KeymanWeb resources saved under $1
    echo
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

if builder_start_action clean; then
  rm -rf "release/"
  rm -rf "intermediate/"
  builder_finish_action success clean
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
if builder_has_action build:embed || builder_has_action build:web; then
  echo "${COLOR_PURPLE}Compiling version ${VERSION}${COLOR_RESET}"
  echo ""
fi

if builder_start_action build:samples; then
  echo "Copying samples & test page resources..."
  cp "${PREDICTIVE_TEXT_SOURCE}" "${PREDICTIVE_TEXT_OUTPUT}"

  builder_finish_action success build:samples;
fi

### -embed section start

if builder_start_action build:embed; then
  $compilecmd -b $SOURCE/tsconfig.embedded.json -v

  assert_exists $INTERMEDIATE/keyman.js

  echo Embedded TypeScript compiled as $INTERMEDIATE/keyman.js

  copy_resources "$INTERMEDIATE"  # Very useful for local testing.

  finish_nominify $EMBEDDED $EMBED_TARGET

  if ! builder_has_option --skip-minify; then
    # Create our entire embedded compilation results path.
    if ! [ -d $EMBED_OUTPUT/resources ]; then
      mkdir -p "$EMBED_OUTPUT/resources"  # Includes base folder, is recursive.
    fi

    if [ -f "$EMBED_OUTPUT/keyman.js" ]; then
      rm $EMBED_OUTPUT/keyman.js 2>/dev/null
    fi

    minify keyman.js $EMBED_OUTPUT SIMPLE_OPTIMIZATIONS
    assert_exists $EMBED_OUTPUT/keyman.js
    echo Compiled embedded application saved as $EMBED_OUTPUT/keyman.js

    # Update any changed resources
    # echo Copy or update resources

    copy_resources "$EMBED_OUTPUT/resources"

    # Update build number if successful
    echo
    echo KMEA/KMEI version $VERSION compiled and saved under $EMBED_OUTPUT
    echo
  fi

  if builder_has_option --debug; then
    # We currently have an issue with sourcemaps for minified versions in embedded contexts.
    # We should use the unminified one instead for now.
    cp $EMBED_OUTPUT_NO_MINI/keyman.js $EMBED_OUTPUT/keyman.js
    # Copy the sourcemap.
    cp $EMBED_OUTPUT_NO_MINI/keyman.js.map $EMBED_OUTPUT/keyman.js.map
    echo Uncompiled embedded application saved as keyman.js
  fi

  # TODO:  handle this block somehow.

  # if [ $UPLOAD_EMBED_SENTRY = true ]; then  # upload-symbols:embed
  #   if [ $BUILD_DEBUG_EMBED = true ]; then
  #     ARTIFACT_FOLDER="release/unminified/embedded"
  #     pushd $EMBED_OUTPUT_NO_MINI
  #   else
  #     ARTIFACT_FOLDER="release/embedded"
  #     pushd $EMBED_OUTPUT
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
  # Compile KeymanWeb code modules for native keymanweb use, stubbing out and removing references to debug functions
  echo Compile Keymanweb...
  $compilecmd -b $SOURCE/tsconfig.json -v
  if [ $? -ne 0 ]; then
    fail "Typescript compilation failed."
  fi
  assert_exists $INTERMEDIATE/keymanweb.js

  echo Native TypeScript compiled as $INTERMEDIATE/keymanweb.js

  copy_resources "$INTERMEDIATE"

  finish_nominify $WEB $WEB_TARGET

  if ! builder_has_option --skip-minify; then
    if [ -f "$WEB_OUTPUT/keymanweb.js" ]; then
      rm $WEB_OUTPUT/keymanweb.js 2>/dev/null
    fi

    echo Minifying KeymanWeb...
    minify keymanweb.js $WEB_OUTPUT SIMPLE_OPTIMIZATIONS
    assert_exists $WEB_OUTPUT/keymanweb.js

    echo Compiled KeymanWeb application saved as $WEB_OUTPUT/keymanweb.js

    copy_resources "$WEB_OUTPUT"
    # Update build number if successful
    echo
    echo KeymanWeb $VERSION compiled and saved under $WEB_OUTPUT
    echo
  fi

  builder_finish_action success build:web
fi

if builder_start_action build:ui; then
  $compilecmd -b $SOURCE/tsconfig.ui.json

  echo "---------- NOTE -----------"
  pwd
  echo "---------- NOTE -----------"

  CURRENT_PATH=`pwd`
  # Since the batch compiler for the UI modules outputs them within a subdirectory,
  # we need to copy them up to the base /intermediate/ folder.
  cd "$INTERMEDIATE/web/$SOURCE"
  cp * ../../
  cd $CURRENT_PATH

  assert_exists $INTERMEDIATE/kmwuitoolbar.js
  assert_exists $INTERMEDIATE/kmwuitoggle.js
  assert_exists $INTERMEDIATE/kmwuifloat.js
  assert_exists $INTERMEDIATE/kmwuibutton.js

  finish_nominify $UI "${UI_TARGET[@]}"

  echo \'Native\' UI TypeScript has been compiled into the $INTERMEDIATE/ folder

  if ! builder_has_option --skip-minify; then
    echo Minify ToolBar UI
    if [ -f "$WEB_OUTPUT/kmuitoolbar.js" ]; then
        rm $WEB_OUTPUT/kmuitoolbar.js 2>/dev/null
    fi
    minify kmwuitoolbar.js $WEB_OUTPUT ADVANCED_OPTIMIZATIONS "web/$SOURCE/" "(function() {%output%}());"
    assert_exists $WEB_OUTPUT/kmwuitoolbar.js

    echo Minify Toggle UI
    if [ -f "$WEB_OUTPUT/kmuitoggle.js" ]; then
        rm $WEB_OUTPUT/kmuitoggle.js 2>/dev/null
    fi
    minify kmwuitoggle.js $WEB_OUTPUT SIMPLE_OPTIMIZATIONS "web/$SOURCE/" "(function() {%output%}());"
    assert_exists $WEB_OUTPUT/kmwuitoggle.js

    echo Minify Float UI
    if [ -f "$WEB_OUTPUT/kmuifloat.js" ]; then
        rm $WEB_OUTPUT/kmuifloat.js 2>/dev/null
    fi
    minify kmwuifloat.js $WEB_OUTPUT ADVANCED_OPTIMIZATIONS "web/$SOURCE/" "(function() {%output%}());"
    assert_exists $WEB_OUTPUT/kmwuifloat.js

    echo Minify Button UI
    if [ -f "$WEB_OUTPUT/kmuibutton.js" ]; then
        rm $WEB_OUTPUT/kmuibutton.js 2>/dev/null
    fi
    minify kmwuibutton.js $WEB_OUTPUT SIMPLE_OPTIMIZATIONS "web/$SOURCE/" "(function() {%output%}());"
    assert_exists $WEB_OUTPUT/kmwuibutton.js

    echo "User interface modules compiled and saved under $WEB_OUTPUT"
  fi

  builder_finish_action success build:ui
fi

if builder_start_action build:tools; then
  tools/build.sh
  builder_finish_action success build:tools
fi

if builder_start_action test:web; then
  if builder_has_option --all; then
    unit_tests/test.sh
  else
    unit_tests/test.sh :engine
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
