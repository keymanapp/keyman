#! /bin/bash
# 
# Compile keymanweb and copy compiled javascript and resources to output/embedded folder
#

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE

WORKING_DIRECTORY=`pwd`

# Trivial change
echo "This is a test"

# This script runs from its own folder
cd "$(dirname "$THIS_SCRIPT")"

display_usage ( ) {
    echo "build.sh [-ui | -test | -embed | -web | -debug_embedded] [-no_minify] [-clean] [-upload-sentry]"
    echo
    echo "  -ui               to compile only desktop user interface modules"
    echo "  -test             to compile for testing without copying resources or" 
    echo "                    updating the saved version number.  Assumes dependencies
                              are unaltered."
    echo "  -embed            to compile only the KMEA/KMEI embedded engine."
    echo "  -web              to compile only the KeymanWeb engine."
    echo "  -debug_embedded   to compile a readable version of the embedded KMEA/KMEI code"
    echo "  -no_minify        to disable the minification '/release/' build sections -"  
    echo "                      the '/release/unminified' subfolders will still be built."
    echo "  -clean            to erase pre-existing build products before the build."
    echo "  -upload-sentry    to upload debug symbols to our Sentry server"
    echo ""
    echo "  If more than one target is specified, the last one will take precedence."
    exit 1
}

# Fails the build if a specified file does not exist.
assert ( ) {
    if ! [ -f $1 ]; then
        fail "Build failed."
        exit 1
    fi
}

# Build products for each main target.
WEB_TARGET=( "keymanweb.js" )
UI_TARGET=( "kmwuibutton.js" "kmwuifloat.js" "kmwuitoggle.js" "kmwuitoolbar.js" )
EMBED_TARGET=( "keyman.js" )

# Variables for the LMLayer
PREDICTIVE_TEXT_SOURCE="../../common/predictive-text/unit_tests/in_browser/resources/models/simple-trie.js"
PREDICTIVE_TEXT_OUTPUT="../testing/prediction-ui/simple-en-trie.js"

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

minified_sourcemap_cleaner="../tools/sourcemap-root"

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

    INPUT="$INTERMEDIATE/$1"
    INPUT_SOURCEMAP="$INPUT.map"
    OUTPUT="$2/$1"
    OUTPUT_SOURCEMAP="$OUTPUT.map"

    # --source_map_location_mapping - maps paths on INPUT source maps for consumption by Closure.
    # ../../.. => keymanapp, ../.. => keymanapp/keyman.  We have TS root sources on 'keyman'.
    $minifycmd --source_map_input "$INPUT|$INPUT_SOURCEMAP" \
        --create_source_map "$OUTPUT_SOURCEMAP" --source_map_include_content \
        --source_map_location_mapping "$INTERMEDIATE|../../.." \
        --js "$INPUT" --compilation_level $3 \
        --js_output_file "$OUTPUT" --warning_level VERBOSE --output_wrapper "$wrapper
//# sourceMappingURL=$1.map"

    # Now to clean the source map.
    assert "$OUTPUT"
    assert "$OUTPUT_SOURCEMAP"

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

clean ( ) {
    rm -rf "../release"
    rm -rf "../intermediate"
}

# Definition of global compile constants
UI="ui"
WEB="web"
EMBEDDED="embedded"

WEB_OUTPUT="../release/web"
EMBED_OUTPUT="../release/embedded"
WEB_OUTPUT_NO_MINI="../release/unminified/web"
EMBED_OUTPUT_NO_MINI="../release/unminified/embedded"
INTERMEDIATE="../intermediate"
SOURCE="."
NODE_SOURCE="source"

SENTRY_RELEASE_VERSION="release-$VERSION_WITH_TAG"

export SENTRY_URL="https://sentry.keyman.com"
export SENTRY_ORG=keyman
export SENTRY_PROJECT=keyman-web
export SENTRY_LOG_LEVEL=info

readonly WEB_OUTPUT
readonly EMBED_OUTPUT
readonly SOURCE

# Ensures that we rely first upon the local npm-based install of Typescript.
# (Facilitates automated setup for build agents.)
PATH="../node_modules/.bin:$PATH"

compiler="npm run tsc --"
compilecmd="$compiler"

# Establish default build parameters
set_default_vars ( ) {
    BUILD_LMLAYER=true
    BUILD_CORE=true
    BUILD_UI=true
    BUILD_EMBED=true
    BUILD_FULLWEB=true
    BUILD_DEBUG_EMBED=false
    BUILD_COREWEB=true
    DO_MINIFY=true
    FETCH_DEPS=true
}

if [[ $# = 0 ]]; then
    FULL_BUILD=true
else
    FULL_BUILD=false
fi

set_default_vars

# Parse args
while [[ $# -gt 0 ]] ; do
    key="$1"
    case $key in
        -ui)
            set_default_vars
            BUILD_LMLAYER=false
            BUILD_EMBED=false
            BUILD_FULLWEB=false
            BUILD_COREWEB=false
            BUILD_CORE=false
            ;;
        -test)
            set_default_vars
            BUILD_LMLAYER=false
            BUILD_TEST=true
            BUILD_UI=false
            BUILD_EMBED=false
            BUILD_FULLWEB=false
            FETCH_DEPS=false
            ;;
        -embed)
            set_default_vars
            BUILD_FULLWEB=false
            BUILD_UI=false
            BUILD_COREWEB=false
            ;;
        -web)
            set_default_vars
            BUILD_EMBED=false
            ;;
        -debug_embedded)
            set_default_vars
            BUILD_EMBED=true
            BUILD_UI=false
            BUILD_COREWEB=false
            BUILD_FULLWEB=false
            BUILD_DEBUG_EMBED=true
            ;;
        -h|-?)
            display_usage
            ;;
        -no_minify)
            DO_MINIFY=false
            ;;
        -clean)
            clean
            ;;
        -upload-sentry)
            # Overrides default value provided by resources/build_utils.sh
            UPLOAD_SENTRY=true
            ;;
    esac
    shift # past argument
done

# No Sentry uploading if not a release build and not directly specified.
# Defaults (that "release build" part) set by resources/build_utils.sh

# `./build.sh` and `./build.sh -embed` calls may upload 'embedded' artifacts to Sentry.
if [[ $UPLOAD_SENTRY = true ]] && [[ $BUILD_EMBED = true ]] && [[ $DO_MINIFY = true ]]; then
    UPLOAD_EMBED_SENTRY=true
else
    UPLOAD_EMBED_SENTRY=false
fi

# `./build.sh` and `./build.sh -web` calls may upload 'web' / 'native' artifacts to Sentry.
if [[ $UPLOAD_SENTRY = true ]] && [[ $BUILD_FULLWEB = true ]] && [[ $DO_MINIFY = true ]]; then
    UPLOAD_WEB_SENTRY=true
else
    UPLOAD_WEB_SENTRY=false
fi

readonly BUILD_LMLAYER
readonly BUILD_UI
readonly BUILD_EMBED
readonly BUILD_FULLWEB
readonly BUILD_DEBUG_EMBED
readonly BUILD_COREWEB
readonly DO_MINIFY
readonly UPLOAD_WEB_SENTRY
readonly UPLOAD_EMBED_SENTRY

if [ $FETCH_DEPS = true ]; then
    # Ensure the dependencies are downloaded.
    verify_npm_setup

    echo "Copying testing resource ${PREDICTIVE_TEXT_SOURCE} to ${PREDICTIVE_TEXT_OUTPUT}"
    cp "${PREDICTIVE_TEXT_SOURCE}" "${PREDICTIVE_TEXT_OUTPUT}" || fail "Failed to copy predictive text model"
fi

if [ $DO_MINIFY = true ]; then
    # NPM install is required for the file to be present.
    if ! [ -f $minifier ];
    then
        echo File $minifier does not exist:  have you set the environment variable \$CLOSURECOMPILERPATH?
        exit 1
    fi

    # Only run if BOTH cases are true b/c it's a minification-focused 'dependency'.
    if [ $FETCH_DEPS = true ]; then
        # Also, build our sourcemap-root tool for cleaning up the minified version's sourcemaps.
        echo "Compiling build tools for minified build products"
        $compiler --build "source/$minified_sourcemap_cleaner/tsconfig.json"
        assert "$minified_sourcemap_cleaner/index.js"
    fi
fi

if [ $BUILD_CORE = true ]; then
    CORE_FLAGS="-skip-package-install"

    # Build the sentry-manager module - it's used in embedded contexts and on one testing page.
    echo "${TERM_HEADING}Compiling KeymanWeb's sentry-manager module...${NORMAL}"
    pushd ../../common/core/web/tools/sentry-manager/src
    ./build.sh $CORE_FLAGS || fail "Failed to compile the sentry-manager module"
    popd
    echo "${TERM_HEADING}sentry-manager module compiled successfully.${NORMAL}"

    if [ $BUILD_LMLAYER = false ]; then
        CORE_FLAGS="$CORE_FLAGS -test"
    fi

    # Ensure that the Input Processor module compiles properly.
    cd ../../common/core/web/input-processor/src
    echo ""
    echo "${TERM_HEADING}Compiling local KeymanWeb dependencies...${NORMAL}"
    ./build.sh $CORE_FLAGS || fail "Failed to compile KeymanWeb dependencies"
    cd $WORKING_DIRECTORY
    echo "${TERM_HEADING}Local KeymanWeb dependency compilations completed successfully.${NORMAL}"
    echo ""
fi

if [ $FULL_BUILD = true ]; then
    echo Compiling version $VERSION
    echo ""
fi

### -embed section start

if [ $BUILD_EMBED = true ]; then
    echo Compile KMEI/KMEA version $VERSION

    $compilecmd -p $NODE_SOURCE/tsconfig.embedded.json
    if [ $? -ne 0 ]; then
        fail "Typescript compilation failed."
    fi
    assert $INTERMEDIATE/keyman.js
    echo Embedded TypeScript compiled as $INTERMEDIATE/keyman.js

    copy_resources "$INTERMEDIATE"  # Very useful for local testing.

    finish_nominify $EMBEDDED $EMBED_TARGET

    if [ $DO_MINIFY = true ]; then
        # Create our entire embedded compilation results path.
        if ! [ -d $EMBED_OUTPUT/resources ]; then
            mkdir -p "$EMBED_OUTPUT/resources"  # Includes base folder, is recursive.
        fi

        if [ -f "$EMBED_OUTPUT/keyman.js" ]; then
            rm $EMBED_OUTPUT/keyman.js 2>/dev/null
        fi

        minify keyman.js $EMBED_OUTPUT SIMPLE_OPTIMIZATIONS
        assert $EMBED_OUTPUT/keyman.js
        echo Compiled embedded application saved as $EMBED_OUTPUT/keyman.js

        # Update any changed resources
        # echo Copy or update resources

        copy_resources "$EMBED_OUTPUT/resources"

        # Update build number if successful
        echo
        echo KMEA/KMEI version $VERSION compiled and saved under $EMBED_OUTPUT
        echo
    fi

    if [ $BUILD_DEBUG_EMBED = true ]; then
        # We currently have an issue with sourcemaps for minified versions in embedded contexts.
        # We should use the unminified one instead for now.
        cp $EMBED_OUTPUT_NO_MINI/keyman.js $EMBED_OUTPUT/keyman.js
        # Copy the sourcemap.
        cp $EMBED_OUTPUT_NO_MINI/keyman.js.map $EMBED_OUTPUT/keyman.js.map
        echo Uncompiled embedded application saved as keyman.js
    fi

    if [ $UPLOAD_EMBED_SENTRY = true ]; then
        if [ $BUILD_DEBUG_EMBED = true ]; then
            ARTIFACT_FOLDER="release/unminified/embedded"
            pushd $EMBED_OUTPUT_NO_MINI
        else
            ARTIFACT_FOLDER="release/embedded"
            pushd $EMBED_OUTPUT
        fi
        echo "Uploading to Sentry..."
        npm run sentry-cli -- releases files "$SENTRY_RELEASE_VERSION" upload-sourcemaps --strip-common-prefix $ARTIFACT_FOLDER --rewrite --ext js --ext map --ext ts || fail "Sentry upload failed."
        echo "Upload successful."
        popd
    fi
fi

### -embed section complete.

if [ $BUILD_COREWEB = true ]; then
    # Compile KeymanWeb code modules for native keymanweb use, stubbing out and removing references to debug functions
    echo Compile Keymanweb...
    $compilecmd -p $NODE_SOURCE/tsconfig.web.json
    if [ $? -ne 0 ]; then
        fail "Typescript compilation failed."
    fi
    assert $INTERMEDIATE/keymanweb.js
    echo Native TypeScript compiled as $INTERMEDIATE/keymanweb.js

    copy_resources "$INTERMEDIATE"

    finish_nominify $WEB $WEB_TARGET

    if [ $DO_MINIFY = true ]; then
        if [ -f "$WEB_OUTPUT/keymanweb.js" ]; then
            rm $WEB_OUTPUT/keymanweb.js 2>/dev/null
        fi

        echo Minifying KeymanWeb...
        minify keymanweb.js $WEB_OUTPUT SIMPLE_OPTIMIZATIONS
        assert $WEB_OUTPUT/keymanweb.js

        echo Compiled KeymanWeb application saved as $WEB_OUTPUT/keymanweb.js
    fi
fi

if [ $BUILD_FULLWEB = true ] && [ $DO_MINIFY = true ]; then
    copy_resources "$WEB_OUTPUT"
    # Update build number if successful
    echo
    echo KeymanWeb $VERSION compiled and saved under $WEB_OUTPUT
    echo
fi

if [ $BUILD_UI = true ]; then
    echo Compile UI Modules...
    $compilecmd -p $NODE_SOURCE/tsconfig.ui.json

    if [ $? -ne 0 ]; then
        fail "Typescript compilation of the UI modules failed."
    fi

    CURRENT_PATH=`pwd`
    # Since the batch compiler for the UI modules outputs them within a subdirectory,
    # we need to copy them up to the base /intermediate/ folder.
    cd "$INTERMEDIATE/source"
    cp * ../
    cd $CURRENT_PATH

    assert $INTERMEDIATE/kmwuitoolbar.js
    assert $INTERMEDIATE/kmwuitoggle.js
    assert $INTERMEDIATE/kmwuifloat.js
    assert $INTERMEDIATE/kmwuibutton.js

    finish_nominify $UI "${UI_TARGET[@]}"

    echo \'Native\' UI TypeScript has been compiled into the $INTERMEDIATE/ folder 

    if [ $DO_MINIFY = true ]; then
        echo Minify ToolBar UI
        if [ -f "$WEB_OUTPUT/kmuitoolbar.js" ]; then
            rm $WEB_OUTPUT/kmuitoolbar.js 2>/dev/null
        fi
        minify kmwuitoolbar.js $WEB_OUTPUT ADVANCED_OPTIMIZATIONS "web/source/" "(function() {%output%}());"
        assert $WEB_OUTPUT/kmwuitoolbar.js

        echo Minify Toggle UI
        if [ -f "$WEB_OUTPUT/kmuitoggle.js" ]; then
            rm $WEB_OUTPUT/kmuitoggle.js 2>/dev/null
        fi
        minify kmwuitoggle.js $WEB_OUTPUT SIMPLE_OPTIMIZATIONS "web/source/" "(function() {%output%}());"
        assert $WEB_OUTPUT/kmwuitoggle.js

        echo Minify Float UI
        if [ -f "$WEB_OUTPUT/kmuifloat.js" ]; then
            rm $WEB_OUTPUT/kmuifloat.js 2>/dev/null
        fi
        minify kmwuifloat.js $WEB_OUTPUT ADVANCED_OPTIMIZATIONS "web/source/" "(function() {%output%}());"
        assert $WEB_OUTPUT/kmwuifloat.js

        echo Minify Button UI
        if [ -f "$WEB_OUTPUT/kmuibutton.js" ]; then
            rm $WEB_OUTPUT/kmuibutton.js 2>/dev/null
        fi
        minify kmwuibutton.js $WEB_OUTPUT SIMPLE_OPTIMIZATIONS "web/source/" "(function() {%output%}());"
        assert $WEB_OUTPUT/kmwuibutton.js

        echo "User interface modules compiled and saved under $WEB_OUTPUT"
    fi
fi

# We can only upload 'web' / 'native' artifacts after ALL are done compiling.
if [ $UPLOAD_WEB_SENTRY = true ]; then
    pushd $WEB_OUTPUT
    echo "Uploading to Sentry..."
    npm run sentry-cli -- releases files "$SENTRY_RELEASE_VERSION" upload-sourcemaps --strip-common-prefix release/web/ --rewrite --ext js --ext map --ext ts || fail "Sentry upload failed."
    echo "Upload successful."
    popd
fi
