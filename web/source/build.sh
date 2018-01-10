#! /bin/bash
# 
# Compile keymanweb and copy compiled javascript and resources to output/embedded folder
#
# Note: any changes to this script should be replicated in build.bat
#

display_usage ( ) {
    echo "build.sh [-ui | -test | -embed | -web | -debug_embedded]"
    echo
    echo "  -ui               to compile desktop user interface modules to output folder"
    echo "  -test             to compile for testing without copying resources or" 
    echo "                    updating the saved version number."
    echo "  -embed            to compile only the KMEA/KMEI embedded engine."
    echo "  -web              to compile only the KeymanWeb engine."
    echo "  -debug_embedded   to compile a readable version of the embedded KMEA/KMEI code"
    exit 1
}

# Fails the build if a specified file does not exist.
assert ( ) {
    if ! [ -f $1 ]; then
        echo "Build failed."
        exit 1
    fi
}

# Ensure the dependencies are downloaded.
echo "Node.js + dependencies check"
npm install

if [ $? -ne 0 ]; then
    fail "Build environment setup error detected!  Please ensure Node.js is installed!"
fi

: ${CLOSURECOMPILERPATH:=../node_modules/google-closure-compiler}
: ${JAVA:=java}

minifier="$CLOSURECOMPILERPATH/compiler.jar"
# We'd love to add the argument --source_map_include_content for distribution in the future,
# but Closure doesn't include the TS sources properly at this time.
minifier_warnings="--jscomp_error=* --jscomp_off=lintChecks --jscomp_off=unusedLocalVariables"
minifycmd="$JAVA -jar $minifier $minifier_warnings --generate_exports"

if ! [ -f $minifier ];
then
    echo File $minifier does not exist:  have you set the environment variable \$CLOSURECOMPILERPATH?
    exit 1
fi

readonly minifier
readonly minifycmd

# $1 - base file name
# $2 - output path
# $3 - optimization level
# $4 - defines
# $5 - additional output wrapper
minify ( ) {
    if [ "$4" ]; then
        defines="--define $4"
    else
        defines=
    fi

    if [ "$5" ]; then
        wrapper=$5
    else
        wrapper="%output%"
    fi

    $minifycmd $defines --source_map_input "$INTERMEDIATE/$1|$INTERMEDIATE/$1.map" \
        --create_source_map $2/$1.map --js $INTERMEDIATE/$1 --compilation_level $3 \
        --js_output_file $2/$1 --warning_level VERBOSE --output_wrapper "$wrapper
//# sourceMappingURL=$1.map"
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
    echo $BUILD > $1/src/version.txt

    cp -Rf $SOURCE/resources/ui  $1/src/ >/dev/null
    cp -Rf $SOURCE/resources/osk $1/src/ >/dev/null

    # Update build number if successful
    echo
    echo KeymanWeb resources saved under $1
    echo
}

# Definition of global compile constants
WEB_OUTPUT="../output"
EMBED_OUTPUT="../embedded"
INTERMEDIATE="../build"
SOURCE="."
NODE_SOURCE="source"

readonly WEB_OUTPUT
readonly EMBED_OUTPUT
readonly SOURCE

# Get build version -- if not building in TeamCity, then always use 300
: ${BUILD_COUNTER:=300}
BUILD=$BUILD_COUNTER

readonly BUILD

# Ensures that we rely first upon the local npm-based install of Typescript.
# (Facilitates automated setup for build agents.)
PATH="../node_modules/.bin:$PATH"

compiler="npm run tsc --"
compilecmd="$compiler"

# Establish default build parameters

BUILD_UI=true
BUILD_EMBED=true
BUILD_FULLWEB=true
BUILD_DEBUG_EMBED=false
BUILD_COREWEB=true

if [[ $# = 0 ]]; then
    FULL_BUILD=true
else
    FULL_BUILD=false
fi

# Parse args
if [[ $# -gt 0 ]] ; then
    key="$1"
    case $key in
        -ui)
            BUILD_EMBED=false
            BUILD_FULLWEB=false
            BUILD_COREWEB=false
            ;;
        -test)
            BUILD_TEST=true
            BUILD_UI=false
            BUILD_EMBED=false
            BUILD_FULLWEB=false
            ;;
        -embed)
            BUILD_FULLWEB=false
            BUILD_UI=false
            BUILD_COREWEB=false
            ;;
        -web)
            BUILD_EMBED=false
            ;;
        -debug_embedded)
            BUILD_EMBED=false
            BUILD_UI=false
            BUILD_COREWEB=false
            BUILD_FULLWEB=false
            BUILD_DEBUG_EMBED=true
            ;;
        -h|-?)
            display_usage
            ;;
    esac
    shift # past argument
fi

readonly BUILD_UI
readonly BUILD_EMBED
readonly BUILD_FULLWEB
readonly BUILD_DEBUG_EMBED
readonly BUILD_COREWEB

if [ $FULL_BUILD = true ]; then
    echo Compiling build $BUILD
    echo ""
fi


if [ $BUILD_EMBED = true ]; then
    echo Compile KMEI/KMEA build $BUILD

    # Create our entire embedded compilation results path.
    if ! [ -d $EMBED_OUTPUT/resources ]; then
        mkdir -p "$EMBED_OUTPUT/resources"  # Includes base folder, is recursive.
    fi

    rm $EMBED_OUTPUT/keyman.js 2>/dev/null
    $compilecmd -p $NODE_SOURCE/tsconfig.embedded.json
    assert $INTERMEDIATE/keyman.js
    echo Typescript compiled.

    minify keyman.js $EMBED_OUTPUT SIMPLE_OPTIMIZATIONS "keyman.__BUILD__=$BUILD"
    assert $EMBED_OUTPUT/keyman.js 

    echo Compiled embedded application saved as $EMBED_OUTPUT/keyman.js

    # Update any changed resources

    # echo Copy or update resources

    cp -Rf $SOURCE/resources $EMBED_OUTPUT/ >/dev/null

    # Update build number if successful
    echo
    echo KMEA/KMEI build $BUILD compiled and saved under $EMBED_OUTPUT
    echo
fi

if [ $BUILD_COREWEB = true ]; then
    # Compile KeymanWeb code modules for native keymanweb use, stubbing out and removing references to debug functions
    echo Compile Keymanweb...
    rm $WEB_OUTPUT/keymanweb.js 2>/dev/null
    $compilecmd -p $NODE_SOURCE/tsconfig.web.json
    assert $INTERMEDIATE/keymanweb.js
    echo Typescript compiled.
    
    copy_resources "$INTERMEDIATE"

    echo Minifying KeymanWeb...
    minify keymanweb.js $WEB_OUTPUT SIMPLE_OPTIMIZATIONS "keyman.__BUILD__=$BUILD"
    assert $WEB_OUTPUT/keymanweb.js

    echo Compiled KeymanWeb application saved as $WEB_OUTPUT/keymanweb.js
fi

if [ $BUILD_FULLWEB = true ]; then
    copy_resources "$WEB_OUTPUT"
    # Update build number if successful
    echo
    echo KeymanWeb 2 build $BUILD compiled and saved under $WEB_OUTPUT
    echo
fi

if [ $BUILD_UI = true ]; then
    echo Compile UI Modules...
    $compilecmd -p $NODE_SOURCE/tsconfig.ui.json
    assert $INTERMEDIATE/kmwuitoolbar.js
    assert $INTERMEDIATE/kmwuitoggle.js
    assert $INTERMEDIATE/kmwuifloat.js
    assert $INTERMEDIATE/kmwuibutton.js

    echo Minify ToolBar UI
    del $WEB_OUTPUT/kmuitoolbar.js 2>/dev/null
    minify kmwuitoolbar.js $WEB_OUTPUT ADVANCED_OPTIMIZATIONS "" "(function() {%output%}());"
    assert $WEB_OUTPUT/kmwuitoolbar.js

    echo Minify Toggle UI
    del $WEB_OUTPUT/kmuitoggle.js 2>/dev/null
    minify kmwuitoggle.js $WEB_OUTPUT SIMPLE_OPTIMIZATIONS "" "(function() {%output%}());"
    assert $WEB_OUTPUT/kmwuitoggle.js

    echo Minify Float UI
    del $WEB_OUTPUT/kmuifloat.js 2>/dev/null
    minify kmwuifloat.js $WEB_OUTPUT ADVANCED_OPTIMIZATIONS "" "(function() {%output%}());"
    assert $WEB_OUTPUT/kmwuifloat.js

    echo Minify Button UI
    del $WEB_OUTPUT/kmuibutton.js 2>/dev/null
    minify kmwuibutton.js $WEB_OUTPUT SIMPLE_OPTIMIZATIONS "" "(function() {%output%}());"
    assert $WEB_OUTPUT/kmwuibutton.js

    echo User interface modules compiled and saved under $WEB_OUTPUT
fi

if [ $BUILD_DEBUG_EMBED = true ]; then
    cp $INTERMEDIATE/keyman.js $EMBED_OUTPUT/keymanios.js
    # Problem - must correct the resulting map location!
    sed -i 's/\/\/# sourceMappingURL=keyman.js.map/\/\/# sourceMappingURL=keymanios.js.map/g' $EMBED_OUTPUT/keymanios.js
    
    # Copy the actual sourcemap.
    cp $INTERMEDIATE/keyman.js.map $EMBED_OUTPUT/keymanios.js.map
    echo Uncompiled embedded application saved as keymanios.js
fi
