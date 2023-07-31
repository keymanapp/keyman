#!/usr/bin/env bash
#
# Compiles Keyman Developer Server for deployment
#

# Exit on command failure and when using unset variables:
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE
EX_USAGE=64

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/resources/build/jq.inc.sh"

pushd "$(dirname "$THIS_SCRIPT")"

# Build the main script.
build () {
  npm run build || builder_die "Could not build top-level JavaScript file."
}

display_usage ( ) {
  echo "Usage: $0 [--production|--watch] [--test|--tdd]"
  echo "       $0 --help"
  echo
  echo "  --help                      displays this screen and exits"
  echo "  --production, -p            builds production release in /developer/bin/server/"
  echo "  --skip-package-install, -S  don't run npm install (not valid with --production)"
  echo "  --test, -t                  runs unit tests after building"
  #echo "  --watch, -w                 builds dev server in watch mode"
  echo "  --tdd                       runs unit tests WITHOUT building"
  echo "  --no-build-addins           don't build/copy Win32 addins"
  echo "  --no-build-kmw              don't build KeymanWeb"
  echo "  --no-copy-kmw               don't copy KeymanWeb"
}

################################ Main script ################################

run_tests=0
production=0
build=1
install_dependencies=1
build_keymanweb=1
copy_keymanweb=1
build_addins=1

# Process command-line arguments
while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    --help|-h|-?)
      display_usage
      exit
      ;;
    --production|-p)
      production=1
      ;;
    --skip-package-install|-S)
      install_dependencies=0
      ;;
    --no-build-kmw)
      build_keymanweb=0
      ;;
    --no-build-addins)
      build_addins=0
      ;;
    --no-copy-kmw)
      copy_keymanweb=0
      ;;
    --test)
      run_tests=1
      ;;
    --tdd)
      run_tests=1
      build=0
      install_dependencies=0
      ;;
    *)
      echo "$0: invalid option: $key"
      display_usage
      exit $EX_USAGE
  esac
  shift # past the processed argument
done

# ----------------------------------------
# Install dependencies
# ----------------------------------------

# Check if Node.JS/npm is installed.
type npm >/dev/null ||\
    builder_die "Build environment setup error detected!  Please ensure Node.js is installed!"

if (( install_dependencies )) ; then
  verify_npm_setup
  # See https://github.com/bubenshchykov/ngrok/issues/254, https://github.com/bubenshchykov/ngrok/pull/255
  # TODO: this is horrible; is there a way we can avoid this?
  rm -f "$KEYMAN_ROOT"/node_modules/ngrok/bin/ngrok.exe
fi

# ----------------------------------------
# Rebuild and bundle addins
# ----------------------------------------

if (( build_addins )); then
  . ./build-addins.inc.sh

  # If we have an x64 version of node installed
  if [[ $(isNodeX64) ]]; then
    build_addins
  fi

  # Build with the Keyman Developer x86 version of node
  PATH="$KEYMAN_ROOT/developer/src/inst/node/dist:$PATH" build_addins
fi

# ----------------------------------------
# Build and bundle KeymanWeb
# ----------------------------------------

if (( build_keymanweb )); then
  pushd "$KEYMAN_ROOT/web/"
  ./build.sh build --debug
  popd
fi

if (( copy_keymanweb )); then
  WEB_SRC="$KEYMAN_ROOT/web/build/publish/debug"
  DST="$(dirname "$THIS_SCRIPT")/src/site/resource"

  rm -rf "$DST"
  mkdir -p "$DST/osk"
  mkdir -p "$DST/ui"
  cp "$WEB_SRC/"*.js "$WEB_SRC/"*.js.map "$DST/"
  cp -R "$WEB_SRC/osk/"* "$DST/osk/"
  cp -R "$WEB_SRC/ui/"* "$DST/ui/"
  cp "$KEYMAN_ROOT/web/LICENSE" "$DST/"
  cp "$KEYMAN_ROOT/web/README.md" "$DST/"
fi

# ----------------------------------------
# Build the project
# ----------------------------------------

npm run build || builder_die "Compilation failed."
echo "Typescript compilation successful."

# ----------------------------------------
# Unit tests
# ----------------------------------------

if (( run_tests )); then
  npm test || builder_die "Tests failed"
fi

# ----------------------------------------
# Deploy to dist/
# ----------------------------------------

if (( production )) ; then
  # We need to build in a tmp folder so that npm doesn't get confused by our
  # monorepo setup, and so we can copy the relevant node_modules in, because
  # we'll need them in order to build the deployable version.

  PRODBUILDTEMP=`mktemp -d`
  echo "Preparing in $PRODBUILDTEMP"
  # Remove @keymanapp devDependencies because they won't install outside the
  # monorepo context
  cat package.json | "$JQ" \
    '. | del(.devDependencies."@keymanapp/resources-gosh") | del(.devDependencies."@keymanapp/keyman-version")' \
    > "$PRODBUILDTEMP/package.json"

  pushd "$PRODBUILDTEMP"
  npm install --omit=dev --omit=optional
  # See https://github.com/bubenshchykov/ngrok/issues/254, https://github.com/bubenshchykov/ngrok/pull/255
  rm -f node_modules/ngrok/bin/ngrok.exe
  popd

  # @keymanapp/keyman-version is required in dist now but we need to copy it in manually
  mkdir -p "$PRODBUILDTEMP/node_modules/@keymanapp/"
  cp -R "$KEYMAN_ROOT/node_modules/@keymanapp/keyman-version/" "$PRODBUILDTEMP/node_modules/@keymanapp/"

  # We'll build in the $KEYMAN_ROOT/developer/bin/server/ folder
  rm -rf "$KEYMAN_ROOT/developer/bin/server/"
  mkdir -p "$KEYMAN_ROOT/developer/bin/server/dist/"
  cp -R dist/* "$KEYMAN_ROOT/developer/bin/server/dist/"
  cp -R "$PRODBUILDTEMP"/* "$KEYMAN_ROOT/developer/bin/server/"
  rm -rf "$PRODBUILDTEMP"
fi
