#! /bin/bash
# 
# Compile keymanweb and copy compiled javascript and resources to output/embedded folder
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

# Definition of global compile constants
WEB_OUTPUT="../output"
EMBED_OUTPUT="../embedded"
SOURCE="."

readonly WEB_OUTPUT
readonly EMBED_OUTPUT
readonly SOURCE

# Get build version -- if not building in TeamCity, then always use 300
: ${BUILD_COUNTER:=300}
BUILD=$BUILD_COUNTER

readonly BUILD

: ${CLOSURECOMPILERPATH:=../tools}
: ${JAVA:=java}

compiler=$CLOSURECOMPILERPATH/compiler.jar
compilecmd="$JAVA -jar $compiler"

if ! [ -f $compiler ];
then
    echo File $compiler does not exist:  have you set the environment variable \$CLOSURECOMPILERPATH?
    exit 1
fi

readonly compiler
readonly compilecmd

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

    # Compile supplementary plane string handing extensions
    echo Compile SMP string extensions
    rm $EMBED_OUTPUT/kmw-smpstring.js 2>/dev/null
    $compilecmd --js $SOURCE/kmwstring.js --compilation_level SIMPLE_OPTIMIZATIONS --js_output_file $EMBED_OUTPUT/kmw-smpstring.js --warning_level VERBOSE
    assert $EMBED_OUTPUT/kmw-smpstring.js

    rm kmwtemp.js 2>/dev/null
    $compilecmd --define __BUILD__=$BUILD --externs $SOURCE/kmwreleasestub.js --js $SOURCE/kmwbase.js --js $SOURCE/keymanweb.js --js $SOURCE/kmwosk.js --js $SOURCE/kmwembedded.js --js $SOURCE/kmwcallback.js --js $SOURCE/kmwkeymaps.js --js $SOURCE/kmwlayout.js --js $SOURCE/kmwinit.js --compilation_level SIMPLE_OPTIMIZATIONS  --js_output_file kmwtemp.js --warning_level VERBOSE
    assert kmwtemp.js 

    echo Append SMP extensions
    cat $EMBED_OUTPUT/kmw-smpstring.js kmwtemp.js > $EMBED_OUTPUT/keyman.js
    rm kmwtemp.js

    echo Compiled embedded application saved as $EMBED_OUTPUT/keyman.js

    # Update any changed resources

    # echo Copy or update resources
    cp -Rf $SOURCE/resources/ $EMBED_OUTPUT/ >/dev/null

    # Update build number if successful
    echo
    echo KMEA/KMEI build $BUILD compiled and saved under $EMBED_OUTPUT
    echo
fi

if [ $BUILD_COREWEB = true ]; then
    # Create our entire compilation results path.  Can't one-line them due to shell-script parsing errors.
    if ! [ -d $WEB_OUTPUT/ui ];      then 
        mkdir -p "$WEB_OUTPUT/ui"      
    fi
    if ! [ -d $WEB_OUTPUT/osk ];     then 
        mkdir -p "$WEB_OUTPUT/osk"     
    fi
    if ! [ -d $WEB_OUTPUT/src/ui ];  then 
        mkdir -p "$WEB_OUTPUT/src/ui"  
    fi
    if ! [ -d $WEB_OUTPUT/src/osk ]; then 
        mkdir -p "$WEB_OUTPUT/src/osk" 
    fi

    # Compile supplementary plane string handing extensions
    echo Compile SMP string extensions
    rm $WEB_OUTPUT/kmw-smpstring.js 2>/dev/null
    $compilecmd --js $SOURCE/kmwstring.js --compilation_level SIMPLE_OPTIMIZATIONS --js_output_file $WEB_OUTPUT/kmw-smpstring.js --warning_level VERBOSE
    assert $WEB_OUTPUT/kmw-smpstring.js 

    # Compile KeymanWeb code modules for native keymanweb use, stubbing out and removing references to debug functions
    echo Compile Keymanweb    
    rm $WEB_OUTPUT/kmwtemp.js 2>/dev/null
    $compilecmd --define __BUILD__=$BUILD --externs $SOURCE/kmwreleasestub.js --js $SOURCE/kmwbase.js --js $SOURCE/keymanweb.js --js $SOURCE/kmwosk.js --js $SOURCE/kmwnative.js --js $SOURCE/kmwcallback.js --js $SOURCE/kmwkeymaps.js --js $SOURCE/kmwlayout.js --js $SOURCE/kmwinit.js --compilation_level SIMPLE_OPTIMIZATIONS  --js_output_file $WEB_OUTPUT/kmwtemp.js --warning_level VERBOSE
    assert $WEB_OUTPUT/kmwtemp.js

    echo Append SMP string extensions to Keymanweb
    cat $WEB_OUTPUT/kmw-smpstring.js $WEB_OUTPUT/kmwtemp.js > $WEB_OUTPUT/keymanweb.js
    rm $WEB_OUTPUT/kmwtemp.js

    echo Compiled KeymanWeb application saved as $WEB_OUTPUT/keymanweb.js
fi

if [ $BUILD_FULLWEB = true ]; then
    echo 
    echo Copy resources to $WEB_OUTPUT/ui, .../osk
    cp -Rf $SOURCE/resources/ui/  $WEB_OUTPUT/  >/dev/null
    cp -Rf $SOURCE/resources/osk/ $WEB_OUTPUT/ >/dev/null

    echo Copy source to $WEB_OUTPUT/src
    cp -Rf $SOURCE/*.js $WEB_OUTPUT/src
    echo $BUILD > $WEB_OUTPUT/src/version.txt
    cp -Rf $SOURCE/resources/ui/  $WEB_OUTPUT/src/ >/dev/null
    cp -Rf $SOURCE/resources/osk/ $WEB_OUTPUT/src/ >/dev/null

    # Update build number if successful
    echo
    echo KeymanWeb 2 build $BUILD compiled and saved under $WEB_OUTPUT
    echo
fi

if [ $BUILD_UI = true ]; then

    echo Compile ToolBar UI
    del $WEB_OUTPUT/kmuitoolbar.js 2>/dev/null
    $compilecmd --js $SOURCE/kmwuitoolbar.js --externs $SOURCE/kmwreleasestub.js --compilation_level ADVANCED_OPTIMIZATIONS --js_output_file $WEB_OUTPUT/kmwuitoolbar.js --warning_level VERBOSE --output_wrapper "(function() {%%output%%}());"
    assert $WEB_OUTPUT/kmwuitoolbar.js

    echo Compile Toggle UI
    del $WEB_OUTPUT/kmuitoggle.js 2>/dev/null
    $compilecmd --js $SOURCE/kmwuitoggle.js --externs $SOURCE/kmwreleasestub.js --compilation_level SIMPLE_OPTIMIZATIONS --js_output_file $WEB_OUTPUT/kmwuitoggle.js --warning_level VERBOSE --output_wrapper "(function() {%%output%%}());"
    assert $WEB_OUTPUT/kmwuitoggle.js

    echo Compile Float UI
    del $WEB_OUTPUT/kmuifloat.js 2>/dev/null
    $compilecmd --js $SOURCE/kmwuifloat.js --externs $SOURCE/kmwreleasestub.js --compilation_level ADVANCED_OPTIMIZATIONS --js_output_file $WEB_OUTPUT/kmwuifloat.js --warning_level VERBOSE --output_wrapper "(function() {%%output%%}());"
    assert $WEB_OUTPUT/kmwuifloat.js

    echo Compile Button UI
    del $WEB_OUTPUT/kmuibutton.js 2>/dev/null
    $compilecmd --js $SOURCE/kmwuibutton.js --externs $SOURCE/kmwreleasestub.js --compilation_level SIMPLE_OPTIMIZATIONS --js_output_file $WEB_OUTPUT/kmwuibutton.js --warning_level VERBOSE --output_wrapper "(function() {%%output%%}());"
    assert $WEB_OUTPUT/kmwuibutton.js

    echo User interface modules compiled and saved under $WEB_OUTPUT
fi

if [ $BUILD_DEBUG_EMBED = true ]; then
    cat $SOURCE/kmwstring.js $SOURCE/kmwbase.js $SOURCE/keymanweb.js $SOURCE/kmwcallback.js $SOURCE/kmwosk.js $SOURCE/kmwembedded.js $SOURCE/kmwkeymaps.js $SOURCE/kmwlayout.js $SOURCE/kmwinit.js > $EMBED_OUTPUT/keymanios.js
    echo Uncompiled embedded application saved as keymanios.js
fi
